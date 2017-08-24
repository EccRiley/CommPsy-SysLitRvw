function lb() {
    var x = document.getElementByClassName('lightbox-target');
    if (x.style.display === 'none') {
        x.style.display = 'block';
    } else {
        x.style.display = 'none';
    }
}

function closeModal() {
  document.getElementByClassName('lightbox-target').style.display = "none";
}
