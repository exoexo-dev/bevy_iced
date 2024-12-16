use bevy_derive::{Deref, DerefMut};
use bevy_ecs::entity::Entity;
use bevy_ecs::prelude::Query;
use bevy_ecs::{
    system::{Commands, Res, Resource},
    world::World,
};
use bevy_render::render_graph::RenderLabel;
use bevy_render::renderer::{RenderDevice, RenderQueue};
use bevy_render::{
    render_graph::{Node, NodeRunError, RenderGraphContext},
    renderer::RenderContext,
    view::ExtractedWindows,
    Extract,
};
use bevy_utils::HashMap;
use iced_wgpu::wgpu::TextureFormat;
use iced_widget::graphics::Viewport;

use crate::{DidDraw, IcedProps, IcedRenderer, IcedRenderers, IcedResource, WindowViewport};

#[derive(Clone, Hash, Debug, Eq, PartialEq, RenderLabel)]
pub struct IcedPass;

pub const TEXTURE_FMT: TextureFormat = TextureFormat::Bgra8UnormSrgb;

/// This resource is used to pass all the viewports attached to windows into
/// the `RenderApp` sub app.
#[derive(Debug, Deref, DerefMut, Clone, Resource)]
pub struct ExtractedIcedWindows(HashMap<Entity, ExtractedIcedWindow>);

#[derive(Debug, Clone)]
pub struct ExtractedIcedWindow {
    viewport: Viewport,
    did_draw: bool,
}

pub(crate) fn extract_iced_data(
    mut commands: Commands,
    windows: Extract<Query<(Entity, &WindowViewport, &DidDraw)>>,
    renderers: Extract<Res<IcedRenderers>>,
) {
    let extracted_windows = windows
        .iter()
        .map(|(window, WindowViewport(viewport), did_draw)| {
            (
                window,
                ExtractedIcedWindow {
                    viewport: viewport.clone(),
                    did_draw: did_draw.swap(false, std::sync::atomic::Ordering::Relaxed),
                },
            )
        })
        .collect();
    commands.insert_resource(ExtractedIcedWindows(extracted_windows));
    commands.insert_resource(renderers.clone());
}

pub fn recall_staging_belt(iced: Res<IcedResource>) {
    iced.lock().unwrap().engine.end_frame();
}

pub struct IcedNode;

impl Node for IcedNode {
    fn run(
        &self,
        _graph: &mut RenderGraphContext,
        render_context: &mut RenderContext,
        world: &World,
    ) -> Result<(), NodeRunError> {
        let windows = &world.get_resource::<ExtractedWindows>().unwrap().windows;
        let ExtractedIcedWindows(extracted_windows) =
            world.get_resource::<ExtractedIcedWindows>().unwrap();

        let IcedProps { engine, .. } = &mut *world.resource::<IcedResource>().lock().unwrap();

        // Render all windows with viewports
        for (window_entity, ExtractedIcedWindow { viewport, did_draw }) in extracted_windows {
            if !did_draw {
                continue;
            }

            let Some(window) = windows.get(window_entity) else {
                continue;
            };
            let render_device = world.resource::<RenderDevice>().wgpu_device();
            let render_queue = world.resource::<RenderQueue>();

            let view = window.swap_chain_texture_view.as_ref().unwrap();

            // TODO: in iced App this is a debug overlay
            let overlay_text: &[String] = &[];

            let renderers = world.resource::<IcedRenderers>();
            let renderer = renderers.get(window_entity);
            match renderer {
                // Nothing to draw in this window if there's no renderer
                None => {
                    continue;
                }
                Some(request_or_use) =>
                // Renderer lock scope
                {
                    let IcedRenderer(renderer) = &mut *request_or_use.lock().unwrap();

                    renderer.present(
                        engine,
                        render_device,
                        render_queue,
                        render_context.command_encoder(),
                        None,
                        TEXTURE_FMT,
                        view,
                        viewport,
                        overlay_text,
                    );
                    engine.finish();
                }
            }
        }

        Ok(())
    }
}
