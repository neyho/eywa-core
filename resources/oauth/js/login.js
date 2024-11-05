let viewbox_target = document.getElementById("background-viewbox")

let image_target = document.getElementById("background-image")
let images = [
  "https://www.eywaonline.com/eywa/images/login_small.png",
  "https://www.eywaonline.com/eywa/images/login_normal.png",
  "https://www.eywaonline.com/eywa/images/login_large.png"
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

// window.addEventListener("resize", fitImage)
// fitImage()


user_input=document.getElementById('username')
password_input=document.getElementById('password')
user_icon=document.getElementById('username-icon')
password_icon=document.getElementById('password-icon')


function track_active (element, icon) {
  element.addEventListener('keyup', (e)=> {
    if (e.target.value != "") {
      icon.classList.add('active')
    } else {
      icon.classList.remove('active')
    }
  })
}


track_active(user_input,user_icon)
track_active(password_input, password_icon)

user_input.focus()

// document.addEventListener("DOMContentLoaded", () => {
//   setTimeout(() => {
//     user_input.focus()
//   }, 500); // Adjust the delay as needed
// });

