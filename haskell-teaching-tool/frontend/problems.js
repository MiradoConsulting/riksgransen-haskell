
function submit(problem, src, dst, hnt) {
    clearResult(dst);
    let input = document.getElementById(src).value
    if (input.trim() != "") {
        fetch('/problem?id=' + problem, {
            method: "POST"
            , body: input
        }).then(response => {
            if (response.redirected) {
                window.location.href = response.url;
            } else {
                response.json().then(resp => decorateResult(resp, dst, hnt));
            }
        });
    }
}

function clearResult(dst) {
    let divDest = document.getElementById(dst);
    divDest.innerHTML = null;
    divDest.style.color = 'black';
}

function decorateResult(resp, dst, hnt) {
    let divDest = document.getElementById(dst);
    let divHint = document.getElementById(hnt);
    if (resp.successful) {
        divDest.style.backgroundColor = '#99FF99';
        divHint.innerHTML = null;
    } else {
        divDest.style.backgroundColor = '#FF9999';
        if (resp.hint != null) {
            divHint.innerHTML = resp.hint;
        }
    }
    divDest.innerHTML = resp.msgs.join('<br/>');
}
