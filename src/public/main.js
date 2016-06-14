function updateProblems(selectedPlatform) {
    var problems = window.platformsAndProblems.find(platformProblems => platformProblems[0] == selectedPlatform)[1];

    $('#problem').empty();
    problems
        .map(problem => $('<option>', { value: problem, text: problem }))
        .forEach(option => $('#problem').append(option))
        ;
        
    $('.selectpicker').selectpicker('refresh');
    
}

$(function() {
    $('#platform')
        .on('hide.bs.select', function() { updateProblems(this.value) })
        .on('hide.bs.select', () => {
            setTimeout(() => $('[data-id=\"problem\"]').trigger('click'), 60)
        })

    $('#problem')
        .on('hide.bs.select', () => {
            var platform = $('#platform').val();
            var problem = $('#problem').val();
            var targetUrl = '/' + platform + '/' + problem;
            
            window.location.href = targetUrl;
        });
});
