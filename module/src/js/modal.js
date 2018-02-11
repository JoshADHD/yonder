Shiny.addCustomMessageHandler("dull:modal", function(msg) {
  if (msg.content) {
    let template = [
      "<div class='modal fade' tabindex=-1 role='dialog'>",
      "</div>"
    ].join("\n");

    let $modal = $(template).html(msg.content);

    Shiny.renderContent(".modal", msg.content);

    $modal.modal();
  }
});
