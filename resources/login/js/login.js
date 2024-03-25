let viewbox_target = document.getElementById("background-viewbox")

let image_target = document.getElementById("background-image")
let images = [
  "images/login_small.png",
  "images/login_normal.png",
  "images/login_large.png"
]

let images_dimensions = [
  [1760, 2640],
  [2560, 3840],
  [4096, 6144]
]

let offsets = [320, 744, 1564]


function fitImage() {
  let window_width = window.innerWidth
  let window_height = window.innerHeight
  let idx
  if (window_width < 1380) {
    idx = 0
  } else if (window_width < 2560) {
    idx = 1
  } else {
    idx = 2
  }
  let target = images[idx]
  let offset = offsets[idx]
  let [width, height] = images_dimensions[idx]
  let available_width = width - window_width;
  let viewBox = `${available_width/2} ${height - window_height - offset} ${window_width} ${window_height}`
  viewbox_target.setAttribute('viewBox', viewBox)
  if (image_target.getAttribute('href') != target) {
    image_target.setAttribute('href', target)
  }
  if (image_target.getAttribute('width') != width) {
    image_target.setAttribute('width', width)
  }
  if (image_target.getAttribute('height') != height) {
    image_target.setAttribute('height', height)
  }

}


window.addEventListener("resize", fitImage)
fitImage()
