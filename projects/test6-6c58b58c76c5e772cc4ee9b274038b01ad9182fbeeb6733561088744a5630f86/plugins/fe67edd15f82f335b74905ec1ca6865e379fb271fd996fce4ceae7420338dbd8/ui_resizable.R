# tags$script(
#     HTML(paste0("
#         $(document).ready(function() {
#           $('#", id, "-figure_div_%widget_id%').resizable({
#             handles: 'e',
#             resize: function(event, ui) {
#               var containerWidth = $('#", id, "-figure_settings_code_div_%widget_id%').width();
#               var leftWidth = ui.size.width / containerWidth * 100;
#               $('#", id, "-figure_div_%widget_id%').css('flex-basis', leftWidth + '%');
#               $('#", id, "-figure_settings_div_%widget_id%').css('flex-basis', (100 - leftWidth) + '%');
#             },
#             stop: function(event, ui) {
#               var containerWidth = $('#", id, "-figure_settings_code_div_%widget_id%').width();
#               var leftWidth = ui.size.width / containerWidth * 100;
#               $('#", id, "-figure_div_%widget_id%').css('width', '');
#               $('#", id, "-figure_div_%widget_id%').css('flex-basis', leftWidth + '%');
#               $('#", id, "-figure_settings_div_%widget_id%').css('flex-basis', (100 - leftWidth) + '%');
#             }
#           });
#         });
#       "))
# )
