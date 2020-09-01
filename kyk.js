var coll = document.getElementsByClassName("collapsible");
var i;

for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this.nextElementSibling;
    if (content.style.maxHeight){
      content.style.maxHeight = null;
    } else {
      content.style.maxHeight = content.scrollHeight + "px";
    } 
  });
}

var coll2 = document.getElementsByClassName("collapsible2");
var i;

for (i = 0; i < coll2.length; i++) {
  coll2[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this.nextElementSibling;
    if (content.style.maxHeight){
      content.style.maxHeight = null;
    } else {
      content.style.maxHeight = content.scrollHeight + "px";
    } 
  });
  coll2[i].click(); 
}

function GetPath(link){
  if (link.includes('github.com/hreplots/U6614/blob/master/')) {
    output = link.split('github.com/hreplots/U6614/blob/master/').pop()
  } else {
    output = link.split('hreplots.github.io/U6614/').pop()
  }
  return 'http://104.248.150.210:2317/?owner=hreplots&repo=U6614&path=' + output
}

function GetDate(link){
  if ((link.includes('hreplots')) & (link.includes('U6614')) & (link.includes('github'))) {
    var Httpreq = new XMLHttpRequest()
    var api_link = GetPath(link)
    Httpreq.open("GET",api_link,false)
    Httpreq.send(null)
    var date = Httpreq.responseText
    var options = {
      hour: 'numeric',
    minute: 'numeric',
    hour12: true,
    timeZone: "America/New_York"
    }

    var local_date = new Date(date).toLocaleDateString('en-US', options)
    var update_date = ' (Last updated on ' + local_date + ' ET' + ')'
    return update_date
  } else {
    return ''
  }
}

var coll3 = document.getElementsByClassName("update");
var i;

for (i = 0; i < coll3.length; i++) {
  coll3[i].textContent += GetDate(coll3[i].href)
}