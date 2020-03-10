function logifySlider(el) {
  // regular number style
  $(el).data("ionRangeSlider").update({
    "prettify": function (num) {
      var exp = Math.pow(10, num);
      var fixed = exp.toFixed(1);
      
      if (fixed == exp) {
        return exp;
      }
      
      return fixed.toLocaleString();
    }
  });
}

$(function() {
  var $document = $(document);
  
  $document.on("shiny:sessioninitialized", function(e) {
    $.extend($.fn.dataTable.ext.classes, {
      sPaging: "dt-paginate btn-toolbar justify-content-end float-right mt-3 ",
      sPageButton: "btn btn-grey",
      sPageButtonActive: "active",
      sPageButtonDisabled: "disabled",
      sInfo: "dt-info font-size-sm float-left mt-3"
    });
    
    $.fn.dataTable.Buttons.defaults.dom.container.className = "btn-group float-right mt-3 mr-3";
    $.fn.dataTable.Buttons.defaults.dom.button.className = "btn btn-grey";
    $.fn.dataTable.Buttons.defaults.dom.collection.className = "dt-button-collection btn-group-vertical w-auto px-0 text-wrap";
    
    $(".logify-slider .js-range-slider").each(function() {
      logifySlider(this);
    });
  });
  
  $document.on("preInit.dt", ".shiny-bound-output", function(e) {
    var $dt = $(e.currentTarget);
    
    $dt.find(".dataTables_length select").addClass("custom-select w-auto");
    $dt.find(".dataTables_filter input").addClass("form-control d-inline w-auto");
  });
  
  $document.on("draw.dt", ".shiny-bound-output", function(e) {
    var $dt = $(e.currentTarget);
    
    $dt.find(".dt-paginate span").addClass("btn-group");
    var $table = $dt.find("table");
    
    if (!$table.parent().is(".table-responsive")) {
      $table.wrap("<div class='table-responsive'></div>"); 
    }
  });
  
  Shiny.addCustomMessageHandler("click.library.sm", function(msg) {
    setTimeout(function() {
      $("#lib-gene_form .yonder-form-submit").click();
    }, 100);
  });
  
/*  $document.on("shiny:recalculated", ".datatables", function(e) {
    $(e.currentTarget).find("table").wrap(function(i) {
      var $this = $(this);
      
      console.log($this);
      
      $this.wrap("<div class='table-responsive'></div>");
    });
  });*/
  
});
