//! # Use Iced UI programs in your Bevy application
//!
//! ```no_run
//! use bevy::prelude::*;
//! use bevy_iced::{
//!     IcedAppExtensions, IcedPlugin,
//!     iced::{Program, program::State}
//! };
//!
//! #[derive(Default)]
//! pub struct Ui {
//!     // Set up your UI state
//! }
//!
//! impl Program for Ui {
//!     // Set up your program logic
//! }
//!
//! pub fn main() {
//!     App::new()
//!         .add_plugins(DefaultPlugins)
//!         .add_plugin(IcedPlugin)
//!         .insert_program(Ui::default())
//!         .add_system(ui_system)
//!         .run();
//! }
//!
//! pub fn ui_system(mut ui_state: NonSendMut<State<Ui>>, /* ... */) {
//!     // Do some work here, then modify your ui state by running
//!     // ui_state.queue_message(..);
//! }
//! ```

use std::any::TypeId;
use std::cell::UnsafeCell;
use std::marker::PhantomData;

use std::sync::Mutex;
use std::{cell::RefCell, sync::Arc};

use crate::render::IcedNode;
use crate::render::{IcedRenderData, ViewportResource};

use bevy::ecs::all_tuples;
use bevy::ecs::event::Event;
use bevy::ecs::system::{
    ReadOnlySystemParamFetch, SystemParam, SystemParamFetch, SystemParamItem, SystemState,
};
use bevy::prelude::{
    Deref, DerefMut, EventWriter, Events, IntoPipeSystem, IntoSystem, NonSendMut, Res, ResMut,
    Resource, System, EventReader,
};
use bevy::render::render_graph::RenderGraph;
use bevy::render::RenderStage;
use bevy::time::Time;
use bevy::utils::HashMap;
use bevy::window::Windows;
use bevy::{
    prelude::{App, Plugin, World},
    render::{
        renderer::{RenderContext, RenderDevice},
        RenderApp,
    },
};
use iced::widget::pane_grid;
use iced::{user_interface, UserInterface};
pub use iced_native as iced;
use iced_native::{Debug, Program, Size};
pub use iced_wgpu;
use iced_wgpu::{wgpu, Viewport};

mod conversions;
mod render;
mod systems;

use program::{BevyIcedProcessor, BevyIcedProgram};
pub use render::IcedSettings;
use systems::IcedEventQueue;

/// The main feature of `bevy_iced`.
/// Add this to your [`App`](`bevy::prelude::App`) by calling `app.add_plugin(bevy_iced::IcedPlugin)`.
pub struct IcedPlugin;

impl Plugin for IcedPlugin {
    fn build(&self, app: &mut App) {
        let default_viewport = Viewport::with_physical_size(Size::new(1600, 900), 1.0);
        let default_viewport = ViewportResource(default_viewport);
        let iced_resource: IcedResource = IcedProps::new(app).into();

        app.add_system(systems::process_input)
            .add_system(render::update_viewport)
            .insert_resource(iced_resource.clone())
            .insert_resource(IcedEventQueue::default())
            .insert_resource(default_viewport.clone());

        let render_app = app.sub_app_mut(RenderApp);
        render_app.insert_non_send_resource(RefCell::new(Vec::<DrawFn>::new()));
        render_app.insert_resource(default_viewport);
        render_app.insert_resource(iced_resource);
        render_app.add_system_to_stage(RenderStage::Extract, render::extract_iced_data);
        // render_app.init_resource::<render::IcedPipeline>();
        setup_pipeline(&mut render_app.world.get_resource_mut().unwrap());
    }
}

type DrawFn = Box<dyn FnMut(&World, &mut RenderContext, &Viewport, &mut render::IcedRenderData)>;

pub struct IcedContext {
    active: Option<TypeId>,
    update_fns: HashMap<TypeId, fn(&mut World)>,
    draw_fns: HashMap<TypeId, fn(&World, &mut RenderContext, &mut render::IcedRenderData)>,
}

struct IcedProps {
    renderer: iced_wgpu::Renderer,
    debug: iced_native::Debug,
    clipboard: iced_native::clipboard::Null,
}

impl IcedProps {
    fn new(app: &App) -> Self {
        let device = app
            .sub_app(RenderApp)
            .world
            .get_resource::<RenderDevice>()
            .unwrap()
            .wgpu_device();
        let format = wgpu::TextureFormat::Bgra8UnormSrgb;

        Self {
            renderer: iced_wgpu::Renderer::new(iced_wgpu::Backend::new(
                device,
                Default::default(),
                format,
            )),
            debug: Debug::new(),
            clipboard: iced_native::clipboard::Null,
        }
    }
}

#[derive(Resource, Clone)]
pub struct IcedResource(Arc<Mutex<IcedProps>>);

impl IcedResource {
    fn lock(&self) -> std::sync::LockResult<std::sync::MutexGuard<IcedProps>> {
        self.0.lock()
    }
}

impl From<IcedProps> for IcedResource {
    fn from(value: IcedProps) -> Self {
        Self(Arc::new(Mutex::new(value)))
    }
}

struct IcedProgramData<T> {
    renderer: iced_wgpu::Renderer,
    debug: iced_native::Debug,
    _phantom: PhantomData<T>,
}

/// A trait that adds the necessary features for an [`App`](`bevy::prelude::App`)
/// to handle Iced.
pub trait IcedAppExtensions {
    /// Insert a new [`Program`](`iced::Program`) and make it accessible as a resource.
    fn insert_program<M, T: Program<Renderer = iced_wgpu::Renderer, Message = M> + 'static>(
        &mut self,
        program: T,
    ) -> &mut Self;
}

struct IcedProgram<M, T>(PhantomData<(M, T)>);

impl<M, T: Program<Renderer = iced_wgpu::Renderer, Message = M> + 'static> IcedProgram<M, T> {
    fn update_fn(world: &mut World) {
        world.resource_scope::<iced_native::program::State<T>, _>(|world, mut state| {
            for ev in &**world.resource::<IcedEventQueue>() {
                state.queue_event(ev.clone());
            }
            let bounds = world.resource::<ViewportResource>().logical_size();

            world.resource_scope::<IcedResource, _>(|world, ctx| {
                let IcedProps {
                    ref mut renderer,
                    ref mut debug,
                    ref mut clipboard,
                    ..
                } = &mut *ctx.lock().unwrap();
                let windows = world.resource::<Windows>();
                if !state.is_queue_empty() {
                    let window = windows.get_primary().unwrap();
                    let cursor_position = window.cursor_position().map_or(
                        iced_native::Point { x: 0.0, y: 0.0 },
                        |p| iced_native::Point {
                            x: p.x * bounds.width / window.width(),
                            y: (window.height() - p.y) * bounds.height / window.height(),
                        },
                    );

                    state.update(
                        bounds,
                        cursor_position,
                        renderer,
                        &iced_wgpu::Theme::Dark,
                        &iced_native::renderer::Style {
                            text_color: iced_native::Color::WHITE,
                        },
                        clipboard,
                        debug,
                    );
                }
            });
        });
    }

    fn draw_fn(world: &World, ctx: &mut RenderContext, data: &mut render::IcedRenderData) {
        let viewport = unsafe { world.get_resource_unchecked_mut::<ViewportResource>() }.unwrap();
        let IcedProps {
            ref mut renderer,
            ref mut debug,
            ..
        } = &mut *world.resource::<IcedResource>().lock().unwrap();

        let device = ctx.render_device.wgpu_device();
        renderer.with_primitives(|backend, primitive| {
            backend.present(
                device,
                data.staging_belt,
                &mut ctx.command_encoder,
                data.view,
                primitive,
                &viewport,
                &debug.overlay(),
            );
        });
    }
}

fn create_state<M, T: Program<Renderer = iced_wgpu::Renderer, Message = M> + 'static>(
    app: &mut App,
    program: T,
) {
    let render_world = &mut app.sub_app_mut(RenderApp).world;
    let bounds = render_world.resource::<ViewportResource>().logical_size();
    let IcedProps {
        ref mut renderer,
        ref mut debug,
        ..
    } = &mut *render_world.non_send_resource_mut::<_>();

    let _state = iced_native::program::State::new(program, bounds, renderer, debug);
}

macro_rules! base_insert_proc {
    ($app:expr, $program:expr, $state_type:ty) => {{
        let device = $app
            .sub_app(RenderApp)
            .world
            .get_resource::<RenderDevice>()
            .unwrap()
            .wgpu_device();
        // let format = wgpu::TextureFormat::bevy_default();
        let format = wgpu::TextureFormat::Bgra8UnormSrgb;
        let mut renderer =
            iced_wgpu::Renderer::new(iced_wgpu::Backend::new(device, Default::default(), format));
        let viewport = Viewport::with_physical_size(Size::new(1600, 900), 1.0);
        let mut debug = Debug::new();
        let mut clipboard = iced_native::clipboard::Null;
        let program = iced_native::program::State::new(
            $program,
            viewport.logical_size(),
            &mut renderer,
            &mut debug,
        );

        let update_data = Arc::new(IcedProgramData::<T> {
            renderer,
            debug,
            _phantom: Default::default(),
        });
        let draw_data = update_data.clone();
        $app.insert_non_send_resource(update_data.clone());

        $app.add_system(
            move |program_state: Option<$state_type>,
                  mut data: NonSendMut<Arc<IcedProgramData<T>>>,
                  windows: Res<Windows>,
                  viewport: Res<ViewportResource>,
                  events: Res<IcedEventQueue>| {
                if let Some(mut state) = program_state {
                    let IcedProgramData::<T> {
                        renderer,
                        debug,
                        _phantom,
                    } = unsafe { get_rc_mut(&mut *data) };

                    for ev in &**events {
                        state.queue_event(ev.clone());
                    }

                    let size = viewport.logical_size();

                    if !state.is_queue_empty() {
                        let window = windows.get_primary().unwrap();
                        let cursor_position = window.cursor_position().map_or(
                            iced_native::Point { x: 0.0, y: 0.0 },
                            |p| iced_native::Point {
                                x: p.x * size.width / window.width(),
                                y: (window.height() - p.y) * size.height / window.height(),
                            },
                        );

                        state.update(
                            viewport.logical_size(),
                            cursor_position,
                            renderer,
                            &iced_wgpu::Theme::Dark,
                            &iced_native::renderer::Style {
                                text_color: iced_native::Color::WHITE,
                            },
                            &mut clipboard,
                            debug,
                        );
                    }
                }
            },
        );

        let draw_fn: DrawFn = Box::new(
            move |_world: &World,
                  ctx: &mut RenderContext,
                  current_viewport: &Viewport,
                  data: &mut IcedRenderData| {
                let IcedProgramData::<T> {
                    renderer,
                    debug,
                    _phantom,
                } = unsafe { get_rc_mut(&draw_data) };

                let device = ctx.render_device.wgpu_device();
                renderer.with_primitives(|backend, primitive| {
                    backend.present(
                        device,
                        data.staging_belt,
                        &mut ctx.command_encoder,
                        data.view,
                        primitive,
                        current_viewport,
                        &debug.overlay(),
                    );
                });
            },
        );

        $app.sub_app_mut(RenderApp)
            .world
            .get_non_send_resource_mut::<RefCell<Vec<DrawFn>>>()
            .unwrap()
            .borrow_mut()
            .push(draw_fn);

        program
    }};
}

impl IcedAppExtensions for App {
    fn insert_program<M, T: Program<Renderer = iced_wgpu::Renderer, Message = M> + 'static>(
        &mut self,
        program: T,
    ) -> &mut Self {
        let resource = base_insert_proc!(self, program, NonSendMut<iced_native::program::State<T>>);
        self.insert_non_send_resource(resource)
    }
}

pub(crate) fn setup_pipeline(graph: &mut RenderGraph) {
    graph.add_node(render::ICED_PASS, IcedNode::new());

    graph
        .add_node_edge(
            bevy::render::main_graph::node::CAMERA_DRIVER,
            render::ICED_PASS,
        )
        .unwrap();
}

// TODO: find a cleaner way to share data between the update and render cycles; this needs to go.
unsafe fn get_rc_mut<'a, T>(rc: &'a Arc<T>) -> &'a mut T {
    let data = &**rc as *const T as *mut T;
    &mut *data
}

type Element<'a> = iced::Element<'a, (), iced_wgpu::Renderer>;

pub(crate) trait Invoker<Params, Out>: 'static {
    fn invoke(&self, params: Params) -> Out;
}

macro_rules! invoker_impl {
    ($($param: ident),*) => {
        impl<$($param,)* Out, F: 'static> Invoker<($($param,)*), Out> for F
        where
            F: Fn($($param,)*) -> Out
        {
            fn invoke(&self, params: ($($param,)*)) -> Out {
                let ($($param,)*) = params;
                (self)($($param,)*)
            }
        }
    };
}
// all_tuples!(invoker_impl, 0, 16, P);

mod program;

#[derive(Default)]
pub struct BevyIcedContext {
    program: Option<Box<dyn std::any::Any>>,
}

fn t1(x: &i32) -> &i32 {
    x
}

// fn t2() -> impl for<'a> Invoker<(&'a i32,), &'a i32> {
//     t1
// }

// trait Retriever<'a, T> {
//     fn data(&self) -> &'a T;
// }

// impl<'a, T: Resource> Retriever<'a, T> for Res<'a, T> {
//     fn data(&self) -> &'a T {
//         unsafe { &*(self.as_ref() as *const _) }
//     }
// }

// fn el_test(state: Res<WindowState>) -> Element {
//     let pane = &state.data().pane;
//     let grid = pane_grid::PaneGrid::new(pane, |pane, state, is_maximized| {
//         pane_grid::Content::new(iced_native::widget::text("text"))
//     });

//     grid.into()
// }

// fn t3() -> impl 'static + for<'a> Invoker<(Res<'a, Time>,), Element<'a>> {
//     el_test
// }

// fn get_params<P, Out>(world: &mut World, f: impl Invoker<<<P as SystemParam>::Fetch as SystemParamFetch>::Item, Out>) -> SystemState<P>
// where
//     P: SystemParam + 'static
// {
//     SystemState::new(world)
// }

fn test(world: &mut World, ctx: &mut BevyIcedContext) {
    // let p = crate::program::BevyIcedProgram {
    //     world_ref: None,
    //     system_state: None,
    //     system: el_test
    // };

    // let b: SystemState<(Res<Time>,)> = get_params(world, el_test);

    // el_test.pipe(|t: bevy::prelude::In<Element>| {});

    // let v: Arc<dyn BevyIcedProcessor> = Arc::new(p);

    // ctx.set(el_test);
}

// fn wtf<F: 'static + for<'a> Fn(Res<'a, Time>) -> Element<'a>>(t: BevyIcedProgram<F, (Res<Time>,)>)
//     -> impl Program<Renderer = iced_wgpu::Renderer, Message = ()> {
//     t
// }

#[derive(SystemParam)]
pub struct Context<'w, 's, Message: Event> {
    context: NonSendMut<'w, BevyIcedContext>,
    viewport: Res<'w, ViewportResource>,
    props: Res<'w, IcedResource>,
    windows: Res<'w, Windows>,
    events: ResMut<'w, IcedEventQueue>,
    messages: EventWriter<'w, 's, Message>,
}

type BIS<M> = iced_native::program::State<program::BIP<M>>;

impl<'w, 's, M: Event + std::fmt::Debug> Context<'w, 's, M> {
    pub fn show<'a>(
        &'a mut self,
        element: impl Into<iced_native::Element<'a, M, iced_wgpu::Renderer>>,
    ) {
        let IcedProps {
            ref mut renderer,
            ref mut debug,
            ref mut clipboard,
        } = &mut *self.props.lock().unwrap();
        let bounds = self.viewport.logical_size();

        let element = element.into();
        // let program = self.context.program.take()
        //     .and_then(|x| x.downcast::<BIS<M>>().ok())
        //     .unwrap_or_else(|| {
        //         let program = iced_native::program::State::new(
        //             program::BIP::new(),
        //             self.viewport.logical_size(),
        //             renderer,
        //             debug,
        //         );

        //         Box::new(program)
        //     });

        let cursor_position = {
            let window = self.windows.get_primary().unwrap();
            let cursor_position =
                window
                    .cursor_position()
                    .map_or(iced_native::Point { x: 0.0, y: 0.0 }, |p| {
                        iced_native::Point {
                            x: p.x * bounds.width / window.width(),
                            y: (window.height() - p.y) * bounds.height / window.height(),
                        }
                    });
            cursor_position
        };

        let mut messages = Vec::<M>::new();
        let cache = user_interface::Cache::default();
        let mut ui = UserInterface::build(element, bounds, cache, renderer);
        let (_, event_statuses) = ui.update(
            self.events.as_slice(),
            cursor_position,
            renderer,
            clipboard,
            &mut messages,
        );

        let theme = iced_wgpu::Theme::Dark;
        let style = iced_native::renderer::Style {
            text_color: iced_native::Color::WHITE,
        };

        ui.draw(renderer, &theme, &style, cursor_position);
    }
}
