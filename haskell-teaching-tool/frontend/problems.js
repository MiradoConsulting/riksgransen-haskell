


function submit(problem, src, dst, hnt) {

    // Clear the response & its style
    let divDest = document.getElementById(dst);
    divDest.innerHTML = null;
    divDest.style.color = 'black';

    let input = document.getElementById(src).value
    if (input.trim() != "") {
      fetch('/problem?id=' + problem, { method: "POST"
                                      , body:   input
                                      })
        .then(response => {
          response.json().then(resp => {
            if(resp.successful) {
                document.getElementById(dst).style.backgroundColor = '#99FF99';
                document.getElementById(hnt).innerHTML = null;
            } else {
                document.getElementById(dst).style.backgroundColor = '#FF9999';
                if(resp.hint != null) {
                    document.getElementById(hnt).innerHTML = resp.hint;
                }
            }
            document.getElementById(dst).innerHTML = resp.msgs.join('<br/>');
          });
        });
    }
}
