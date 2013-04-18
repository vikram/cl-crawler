(in-package :com.crawler)

(5am:def-suite :com.crawler.tests)

(5am:in-suite :com.crawler.tests)

(5am:test trim-funny-test
  (5am:is (equal "hello

                               world!"  (trim "hello

                               world!" )))
  (5am:is (equal "hello  @                               world"
                 (trim-funny "hello  @                               world!   "))))


(5am:test replace-funny-test
  (5am:is (equal "hello world" (replace-funny "hello

                               world!")))
  (5am:is (equal "hello#world" (replace-funny "hello

                               world!" :punctuation nil :with "#")))
  (5am:is (equal (unescape-string "&lt; abcd efgh") "< abcd efgh")))

(defparameter *id-test-page-string*
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
                    \"http://www.w3.org/TR/html4/loose.dtd\">
<html>
<head>
  <script src=\"http://code.jquery.com/jquery-latest.js\"></script>
<meta name=\"search_date\" content=\"2006-08-18\" />
  <script>
  $(document).ready(function(){
    $(\"#myDiv\").css(\"border\",\"3px solid red\");
  });
  </script>
  <style>
  div {
    width: 90px;
    height: 90px;
    float:left;
    padding: 5px;
    margin: 5px;
    background-color: #EEEEEE;
  }
  </style>
</head>
<body>
  <div id=\"notMe\"><p>id=\"notMe\"</p></div>
  <div id=\"myDiv\">id=\"myDiv\"</div>
  <div class=\"notThis\">div class=\"notThis\"</div>
  <div class=\"myClass\">div class=\"myClass\"</div>
  <span class=\"myClass\">span class=\"myClass\"</span>
<span id=\"main\">
    <div></div>
    <button>Child</button>
    <div class=\"mini\"></div>
    <div>
      <div class=\"mini\"></div>
      <div class=\"mini\"></div>
    </div>
    <div><button>Grand</button></div>
    <div><span>A Span <em>in</em> child</span></div>
    <span>A Span in main</span>
  </span>
<div></div>
<div id=\"empty\"></div>
<table style=\"display: none;\">
    <tr><td>Row 1</td></tr>
    <tr><td>Row 2</td></tr>
    <tr><td>Row 3</td></tr>
  </table>
<input type=\"checkbox\" name=\"a\" />
    <span>Mary</span>
  </div>
  <div style=\"position: absolute; top: 0px; left: 0px; height: 0px; width: 0px;\">
    <input type=\"checkbox\" name=\"b\" id=\"somecheck2\"/>
    <span>Paul</span>
  </div>
  <div style=\"background-color: white; color: green;\">
    <input type=\"checkbox\" id=\"somecheck1\" name=\"c\" checked=\"checked\" />
    <span>Peter</span>
  </div>
<table border=\"1\">
    <tr><td>Row with Index #0</td></tr>
    <tr><td>Row with Index #1</td></tr>
    <tr><td>Row with Index #2</td></tr>
    <tr><td>Row with Index #3</td></tr>
  </table>
<ul>
                                <li class=\"simple\" id=\"n-jQuery-Core\"><a href=\"/Core\">jQuery Core</a></li>
                                <li id=\"n-Selectors\"><a href=\"/Selectors\">Selectors</a></li>
                                <li id=\"n-Attributes\"><a href=\"/Attributes\">Attributes</a></li>
                                <li id=\"n-Traversing\"><a href=\"/Traversing\">Traversing</a></li>
                                <li id=\"n-Manipulation\"><a href=\"/Manipulation\">Manipulation</a></li>
                                <li id=\"n-CSS\"><a href=\"/CSS\">CSS</a></li>
                                <li id=\"n-Events\"><a href=\"/Events\">Events</a></li>
                                <li id=\"n-Effects\"><a href=\"/Effects\">Effects</a></li>
                                <li class=\"simple\" id=\"n-Ajax\"><a href=\"/Ajax\">Ajax</a></li>
                                <li id=\"n-Utilities\"><a class=\"lin\" id=\"utilitieslink\" href=\"/Utilities\">Utilities</a></li>
                                <li class=\"involved\" id=\"n-jQuery-UI\"><a id=\"jquerycorelink\" class=\"lin\" href=\"/UI\">jQuery UI</a></li>
                                <li id=\"n-Effects\"><a><img src=\"/Effects\" alt=\"Effects\"/></a></li>
                        </ul>
<div id=\"adstorage2\" style=\"display: inline;\"><script src=\"http://fe.lea.lycos.fr/DartRichMedia_1_03.js\"/>
<!-- Sniffer Code for Flash version=70 -->
<object width=\"300\" height=\"250\" id=\"flashad\" codebase=\"http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=7,0,0,0\" classid=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\"> </object>
<noscript>
<a href=\"http://fe.lea.lycos.fr/event.ng/Type=click&FlightID=317156&AdID=605991&TargetID=106816&Targets=100879,106295,106954,109138,81284,96450,107454,109149,108833,105520,108286,108724,107796,109165,102283,108087,108902,106816,107545,108173,109278&Values=30,51,63,73,84,102,110,150,190,214,224,243,288,9498,9530,13919,16647,23804,50784,63993,87724,88070,88159,126641,168718,197580,232190,366449,412823,412862,412942,412971,412985,412987,412988,413000,523532,541907,542331,542545,543982,544008,549460,550619,554992,556412,556832,558199,558880&Redirect=http://www.loterie.lycos.fr/inscription.php?id=168&idc=373\" target=\"_blank\"><img src=\"http://www.lycos.de/advertising/graphics/_lycos/trans1x1.gif\" width=\"300\" height=\"250\" border=\"0\" alt=\"\" /></a>
</noscript></div>
</body>
</html>")

(defparameter *empty-test-page-string*
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
                    \"http://www.w3.org/TR/html4/loose.dtd\">
<html>
<head>
</head>
<body>
  <div id=\"notMe\"></div>
<script>
<!--
function tn15resize(timeout, chain) {
  var timer;
  return function() {
    window.clearTimeout(timer);
    timer = window.setTimeout(function() { try {
      var w;
      if (self.innerWidth) w = self.innerWidth;
      else if (document.documentElement && document.documentElement.clientWidth) w = document.documentElement.clientWidth;
      else if (document.body) w = document.body.clientWidth;
      document.getElementById('tn15content').className = w < 950 ? 'thin' : 'wide';
    } catch (e) { } }, timeout);
    if (chain) chain();
  }
}
window.onload = tn15resize(1000, window.onload);
window.onresize = tn15resize(100, window.onresize);
(tn15resize(100))();
//-->
</script>

<style type=\"text/css\">
.media_strip_thumbs {
  overflow: hidden;
  height: 90px;
}
.media_strip_thumbs img {
  margin-right: 0.2em;
}
</style>
<div></div>
</body>
</html>")


(defparameter *broken-test-page-string*
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
                    \"http://www.w3.org/TR/html4/loose.dtd\">
<html>
<head>
  <script src=\"http://code.jquery.com/jquery-latest.js\"></script>

  <script>
  $(document).ready(function(){
    $(\"#myDiv\").css(\"border\",\"3px solid red\");
  });
  </script>
  <style>
  div {
    width: 90px;
    height: 90px;
    float:left;
    padding: 5px;
    margin: 5px;
    background-color: #EEEEEE;
  }
</head>
<body>
  <div id=\"notMe\"><p>id=\"notMe\"</p></div>
  <div id=\"myDiv\">id=\"myDiv\"</div>
  <div class=\"notThis\">div class=\"notThis\"</div>
  <div class=\"myClass\">div class=\"myClass\"</div>
  <span class=\"myClass\">span class=\"myClass\"
<span id=\"main\">
    <div></div>
    <button>Child</button>
    <div class=\"mini\">
    <div>
      <div class=\"mini\"></div>
      <div class=\"mini\"></div>
    </div>
    <div><button>Grand</button></div>
    <div><span>A Span <em>in</em> child</span></div>
    <span>A Span in main
  </span>
<table>
    <tr><td>Row 1</td></tr>
    <tr><td>Row 2</td></tr>
    <tr><td>Row 3</td>
  </table>
<input type=\"checkbox\" name=\"a\" />
    <span>Mary</span>
  </div>
  <div>
    <input type=\"checkbox\" name=\"b\" id=\"somecheck2\"/>
    <span>Paul</span>
  </div>
  <div>
    <input type=\"checkbox\" id=\"somecheck1\" name=\"c\" checked=\"checked\" />
    <span>Peter</span>
  </div>
<table border=\"1\">
    <tr><td>Row with Index #0</td></tr>
    <tr><td>Row with Index #1</td></tr>
    <tr><td>Row with Index #2</td></tr>
    <tr><td>Row with Index #3</tr>
  </table>
<ul>
                                <li class=\"simple\" id=\"n-jQuery-Core\"><a href=\"/Core\">jQuery Core</a></li>
                                <li id=\"n-Selectors\"><a href=\"/Selectors\">Selectors</a></li>
                                <li id=\"n-Attributes\"><a href=\"/Attributes\">Attributes</a></li>
                                <li id=\"n-Traversing\"><a href=\"/Traversing\">Traversing</a></li>
                                <li id=\"n-Manipulation\"><a href=\"/Manipulation\">Manipulation</a></li>
                                <li id=\"n-CSS\"><a href=\"/CSS\">CSS</a></li>
                                <li id=\"n-Events\"><a href=\"/Events\">Events</a>
                                <li id=\"n-Effects\"><a href=\"/Effects\">Effects</a></li>
                                <li class=\"simple\" id=\"n-Ajax\"><a href=\"/Ajax\">Ajax</a></li>
                                <li id=\"n-Utilities\"><a class=\"lin\" id=\"utilitieslink\" href=\"/Utilities\">Utilities</a></li>
                                <li class=\"involved\" id=\"n-jQuery-UI\"><a id=\"jquerycorelink\" class=\"lin\" href=\"/UI\">jQuery UI</a></li>
                        </ul>
</body>
</html>")

(defparameter *id-test-page* (html->list *id-test-page-string*))

(defparameter *oreilly-test-page*
"
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" id=\"sixapart-standard\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
<title>Round 2:  Dial Tone - O'Reilly Radar</title>
<meta name=\"generator\" content=\"Movable Type 4.1\" />
<meta name=\"keywords\" content=\"\" />
<meta name=\"description\" content=\"In a conversation yesterday, John Fandel, the general manager of The O'Reilly Network, made an interesting point: he wants to build our web publishing tools around the model of delivering \"dial tone.\" As we talked, the idea took hold. I was reminded of Michael Crichton's observation in his 1983 book Electronic Life that in the 1940's there was concern that...\" />

<meta name=\"search_date\" content=\"2006-08-18\" />
<meta name=\"author\" content=\"Tim O'Reilly\" />


<link rel=\"stylesheet\" href=\"http://radar.oreilly.com/radar.css\" type=\"text/css\" />
<link rel=\"apple-touch-icon\" href=\"/apple-touch-icon.png\" type=\"image/png\" />
<!--mt:If name=\"main_template\"-->

<link rel=\"alternate\" type=\"application/atom+xml\" title=\"Syndication Feed\" href=\"http://radar.oreilly.com/feed\" />
<link rel=\"alternate\" type=\"application/rss+xml\" title=\"RSS\" href=\"http://radar.oreilly.com/rss.xml\" />
<!--/mt:If-->

<!--mt:If name=\"main_index\"-->
<link rel=\"EditURI\" type=\"application/rsd+xml\" title=\"RSD\" href=\"http://radar.oreilly.com/rsd.xml\" />
<!--/mt:If-->


<link rel=\"start\" href=\"http://radar.oreilly.com/\" title=\"Home\" />




<link rel=\"prev\" href=\"http://radar.oreilly.com/archives/2006/08/why-use-anything-else.html\" title=\"Why use anything else?\" />
<link rel=\"next\" href=\"http://radar.oreilly.com/archives/2006/08/paul-yanover-new-head-of-disne.html\" title=\"Paul Yanover new head of Disney Online\" />


<!--
<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
         xmlns:trackback=\"http://madskills.com/public/xml/rss/module/trackback/\"
         xmlns:dc=\"http://purl.org/dc/elements/1.1/\">
<rdf:Description
    rdf:about=\"http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\"
    trackback:ping=\"http://radar.oreilly.com/mt/mt-tb.cgi/8449\"
    dc:title=\"Round 2:  Dial Tone\"
    dc:identifier=\"http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\"
    dc:subject=\"Thought-provoking\"
    dc:description=\"In a conversation yesterday, John Fandel, the general manager of The O&apos;Reilly Network, made an interesting point: he wants to build our web publishing tools around the model of delivering &quot;dial tone.&quot; As we talked, the idea took hold. I was reminded of Michael Crichton&apos;s observation in his 1983 book Electronic Life that in the 1940&apos;s there was concern that...\"
    dc:creator=\"Tim O&apos;Reilly\"
    dc:date=\"2006-08-18T05:45:32-08:00\" />
</rdf:RDF>
-->

<script type=\"text/javascript\" src=\"http://radar.oreilly.com/mt.js\"></script>


<!--
<rdf:RDF xmlns=\"http://web.resource.org/cc/\"
         xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
         xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">
<Work rdf:about=\"http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\">
<dc:title>Round 2:  Dial Tone</dc:title>
<dc:description>In a conversation yesterday, John Fandel, the general manager of The O&apos;Reilly Network, made an interesting point: he wants to build our web publishing tools around the model of delivering &quot;dial tone.&quot; As we talked, the idea took hold. I was reminded of Michael Crichton&apos;s observation in his 1983 book Electronic Life that in the 1940&apos;s there was concern that...</dc:description>
<dc:creator>tim</dc:creator>
<dc:date>2006-08-18T05:45:32-08:00</dc:date>
<license rdf:resource=\"http://creativecommons.org/licenses/by-nc-sa/1.0/\" />
</Work>
<License rdf:about=\"http://creativecommons.org/licenses/by-nc-sa/1.0/\">
<requires rdf:resource=\"http://web.resource.org/cc/Attribution\" />
<requires rdf:resource=\"http://web.resource.org/cc/Notice\" />
<requires rdf:resource=\"http://web.resource.org/cc/ShareAlike\" />
<permits rdf:resource=\"http://web.resource.org/cc/Reproduction\" />
<permits rdf:resource=\"http://web.resource.org/cc/Distribution\" />
<permits rdf:resource=\"http://web.resource.org/cc/DerivativeWorks\" />
<prohibits rdf:resource=\"http://web.resource.org/cc/CommercialUse\" />
</License>
</rdf:RDF>
-->


<script type=\"text/javascript\" src=\"/scripts/multi_bucket.js\"></script>
<script type=\"text/javascript\" src=\"/scripts/lightbox.js\"></script>
<script type=\"text/javascript\" src=\"/scripts/nav.js\"></script>

<script type=\"text/javascript\" src=\"/scripts/prototype.js\"></script>
<script type=\"text/javascript\" src=\"/scripts/scriptaculous.js\"></script>
<script defer=\"defer\" type=\"text/javascript\" src=\"http://www.sphere.com/widgets/sphereit/js?siteid=oreilly\"></script>
<!-- removed, lents -->
<!-- mt:GeoPressHeader / -->


</head>
<body class=\"mt-comment-confirmation layout-wt\" onload=\"individualArchivesOnLoad(commenter_name)\">
<div id=\"sizer\"><!-- The SIZER takes a percentage of the space left by the BODY -->
<div id=\"expander\"><!-- The EXPANDER uses negative margins to push out the sides and take back the space lost to BODY padding -->
<div id=\"wrapper\"><!-- The WRAPPER is set to width:100%. This helps IE get the percentage width of CONTENT and SIDEBAR correct -->
<div id=\"container\">
<div id=\"container-inner\">

<div id=\"header\">
<div id=\"header-inner\">
<div id=\"header-content\">

<div id=\"global-nav\" class=\"noindex\">
<div class=\"right\">
<script language=\"JavaScript\" type=\"text/javascript\">
        <!--
        var name = \"radar-contact\";
        var domain = \"oreilly.com\";
        var display = \"CONTACT RADAR\";
        var subject = \"\";
        var ending = \"\";
        var style = \"\";
        document.write('<a class=\"' + style + '\" href=\"mailto:' + name + '&#64;' + domain + '?subject=' + subject + '\">');
        if (display) {
         document.write(display);
        } else {
         document.write(name + '&#64;' + domain);
        }
        document.write('</a>' + ending);
        document.write('<!-- mailmunge bit -->');
        // -->
</script> &nbsp;&#8226;&nbsp; <a href=\"http://www.oreilly.com\">O'REILLY HOME</a></div>

<ul>

<li><a href=\"http://radar.oreilly.com/\">RADAR</a></li>

<li><a href=\"http://radar.oreilly.com/r2/\">RELEASE 2.0</a></li>


<li><a href=\"http://radar.oreilly.com/research/\">RESEARCH</a></li>


<li><a href=\"http://radar.oreilly.com/conferences/\">CONFERENCES</a></li>


<li><a href=\"http://radar.oreilly.com/about/\">ABOUT</a></li>

</ul>

</div>

<div id=\"sub-nav\" class=\"noindex\">
<div class=\"right\">
<form id=\"search-form\" name=\"searchForm\" method=\"get\" action=\"http://radar.oreilly.com/mt/mt-search.cgi\">
<input type=\"hidden\" name=\"IncludeBlogs\" value=\"27\" />
<ul>
<li><input id=\"search\" name=\"search\" type=\"text\" size=\"12\" />
<input type=\"submit\" value=\"Search\" alt=\"Submit\" onclick=\"\" /></li>
</ul>
</form>
</div>

<ul>
<li><a href=\"http://radar.oreilly.com/\">All</a></li>

<li><a href=\"http://radar.oreilly.com/open_source/\">Open Source</a></li>
<li><a href=\"http://radar.oreilly.com/geo/\">Geo</a></li>
<li><a href=\"http://radar.oreilly.com/emerging_tech/\">Emerging Tech</a></li>
<li><a href=\"http://radar.oreilly.com/web2/\">Web 2.0</a></li>
<li><a href=\"http://radar.oreilly.com/operations/\">Operations</a></li>
<li><a href=\"http://radar.oreilly.com/videos/\">Videos</a></li>
<li><a href=\"http://radar.oreilly.com/events/\">Events</a></li>
</ul>







</div>
<div class=\"logo\">
<a href=\"http://radar.oreilly.com/\"><img src=\"/images/radar_logo.gif\" width=\"264\" height=\"61\" alt=\"O'Reilly Radar Logo\" /></a>
</div>

<div class=\"flickr_photos\">
<script type=\"text/javascript\" language=\"javascript\">
<!--
<!-- flickrPool should be given the id of a flickr pool. It will default to 21711495%40N00 -->
        var flickrPool = '21711495%40N00';
        var flickrPoolName = \"radar\";

        <!-- tags should be added in a space-separated list. For example: 'oreilly oscon 2007' -->
        var flickrTags = '';

        var oeTags = '<object classid=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\"'
        + 'width=\"400\" height=\"70\"'
        + 'codebase=\"http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab\">'
        + '<param name=\"movie\" value=\"/flash/flickr_photos.swf?flickrPool=' + flickrPool + '&flickrTags=' + flickrTags + '&flickrPoolName=' + flickrPoolName + '\" /><param name=\"menu\" value=\"false\" /><param name=\"quality\" value=\"high\" /><param name=\"wmode\" value=\"transparent\" /><param name=\"bgcolor\" value=\"#ffffff\" />'
        + '<embed src=\"/flash/flickr_photos.swf?flickrPool=' + flickrPool + '&flickrTags=' + flickrTags + '&flickrPoolName=' + flickrPoolName + '\" menu=\"false\" quality=\"high\" wmode=\"transparent\" bgcolor=\"#ffffff\" '
        + 'width=\"400\" height=\"70\" name=\"flickr_photos\" align=\"middle\"'
        + 'play=\"true\"'
        + 'loop=\"false\"'
        + 'quality=\"high\"'
        + 'allowScriptAccess=\"sameDomain\"'
        + 'type=\"application/x-shockwave-flash\"'
        + 'pluginspage=\"http://www.macromedia.com/go/getflashplayer\">'
        + '<\/embed>'
        + '<\/object>';
        document.write(oeTags);   // embed the flash movie
        // -->
</script>
</div>
<div class=\"clear_float\">&nbsp;</div>

</div> <!-- /header-content -->
</div> <!-- /header-inner -->
</div> <!-- /header -->
<div id=\"content\">
<div id=\"content-inner\">
<div id=\"alpha\">
<div id=\"alpha-inner\">



<!-- alpha-inner content -->

<script type=\"text/javascript\" language=\"javascript\">
<!--

var HOST = 'radar.oreilly.com';

// Copyright (c) 1996-1997 Athenia Associates.
// http://www.webreference.com/js/
// License is granted if and only if this entire
// copyright notice is included. By Tomer Shiran.

function setCookie (name, value, expires, path, domain, secure) {
    var curCookie = name + \"=\" + escape(value) + (expires ? \"; expires=\" + expires : \"\") + (path ? \"; path=\" + path : \"\") + (domain ? \"; domain=\" + domain : \"\") + (secure ? \"secure\" : \"\");
    document.cookie = curCookie;
}

function getCookie (name) {
    var prefix = name + '=';
    var c = document.cookie;
    var nullstring = '';
    var cookieStartIndex = c.indexOf(prefix);
    if (cookieStartIndex == -1)
        return nullstring;
    var cookieEndIndex = c.indexOf(\";\", cookieStartIndex + prefix.length);
    if (cookieEndIndex == -1)
        cookieEndIndex = c.length;
    return unescape(c.substring(cookieStartIndex + prefix.length, cookieEndIndex));
}

function deleteCookie (name, path, domain) {
    if (getCookie(name))
        document.cookie = name + \"=\" + ((path) ? \"; path=\" + path : \"\") + ((domain) ? \"; domain=\" + domain : \"\") + \"; expires=Thu, 01-Jan-70 00:00:01 GMT\";
}

function fixDate (date) {
    var base = new Date(0);
    var skew = base.getTime();
    if (skew > 0)
        date.setTime(date.getTime() - skew);
}

function rememberMe (f) {
    var now = new Date();
    fixDate(now);
    now.setTime(now.getTime() + 365 * 24 * 60 * 60 * 1000);
    now = now.toGMTString();
    if (f.author != undefined)
       setCookie('mtcmtauth', f.author.value, now, '/', '', '');
    if (f.email != undefined)
       setCookie('mtcmtmail', f.email.value, now, '/', '', '');
    if (f.url != undefined)
       setCookie('mtcmthome', f.url.value, now, '/', '', '');
}

function forgetMe (f) {
    deleteCookie('mtcmtmail', '/', '');
    deleteCookie('mtcmthome', '/', '');
    deleteCookie('mtcmtauth', '/', '');
    f.email.value = '';
    f.author.value = '';
    f.url.value = '';
}

//-->

</script>

<!--
<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
         xmlns:trackback=\"http://madskills.com/public/xml/rss/module/trackback/\"
         xmlns:dc=\"http://purl.org/dc/elements/1.1/\">
<rdf:Description
    rdf:about=\"http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\"
    trackback:ping=\"http://radar.oreilly.com/mt/mt-tb.cgi/8449\"
    dc:title=\"Round 2:  Dial Tone\"
    dc:identifier=\"http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\"
    dc:subject=\"Thought-provoking\"
    dc:description=\"In a conversation yesterday, John Fandel, the general manager of The O&apos;Reilly Network, made an interesting point: he wants to build our web publishing tools around the model of delivering &quot;dial tone.&quot; As we talked, the idea took hold. I was reminded of Michael Crichton&apos;s observation in his 1983 book Electronic Life that in the 1940&apos;s there was concern that...\"
    dc:creator=\"Tim O&apos;Reilly\"
    dc:date=\"2006-08-18T05:45:32-08:00\" />
</rdf:RDF>
-->


<div class=\"post_block\">
<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>
</div> <!-- top -->
<div class=\"mid\">
<div class=\"left\">
<h3>Fri</h3>
<p class=\"date\">
Aug 18<br />

2006
</p>
<a href=\"/tim\"><img src=\"/images/people/photo_tim_s.jpg\" width=\"57\" alt=\"Tim O'Reilly\" /></a>
<p><a href=\"/tim\">Tim O'Reilly</a></p>
</div> <!-- left -->

<!-- ISI_LISTEN_START -->
<span class=ISI_MESSAGE><!-- This weblog is brought to you by the O'Reilly Radar --></span>

<!-- Speakers -->
<span class=\"ISI_IGNORE\">
<ul class=\"listen\">
<!-- <li class=\"option\"><a href=\"http://asp2.readspeaker.net/cgi-bin/radarrsone?lang=us&voice=Paul&customerid=176&url=http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\">listen to all posts</a></li> -->
<li class=\"button\"><a target=\"_new\" href=\"http://asp2.readspeaker.net/cgi-bin/radarrsone?lang=us&voice=Paul&customerid=176&url=http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\">listen <img src=\"/images/icon_listen_small.gif\" width=\"10\" height=\"9\" alt=\"Speech Icon\" /></a></li>

</ul>
</span>
<!-- end speakers -->

<div class=\"right\">
<div class=\"content\">
<h3><a href=\"http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\" target=\"_self\" class=\"title\">Round 2:  Dial Tone</a></h3>
<p>
<p>In a conversation yesterday, <a href=http://www.linkedin.com/pub/0/a33/087>John Fandel</a>, the general manager of <a href=http://www.oreillynet.com>The O'Reilly Network</a>, made an interesting point:  he wants to build our web publishing tools around the model of delivering \"dial tone.\"</p>

<p>As we talked, the idea took hold.  I was reminded of Michael Crichton's observation in his 1983 book <a href=http://www.crichton-official.com/life/life_main.shtml>Electronic Life</a> that in the 1940's there was concern that the telephone system was growing so fast that there wouldn't be enough operators unless AT&T hired every person in America.  AT&T solved the problem by creating automated switching systems that, in effect, did turn every person in the world into an operator--without hiring them.  The principle of dial tone is to create a situation where users can do something for themselves that once required the intervention of an operator.</p>

<p>Dial-tone is also a fabulous metaphor for one of the key principles of Web 2.0, which I've called \"<a href=http://www.oreillynet.com/pub/a/oreilly/tim/articles/architecture_of_participation.html>the architecture of participation</a>,\" but which might also simply be described as the design of systems that leverage customer self-service.  (<a href=http://www.warburgpincus.com/people/profiles/janeway_william.html>Bill Janeway</a> made this linkage to customer self-service as a key driver of success in the internet economy in a presentation he gave at the O'Reilly Emerging Technology Conference in 2004.  Mitch Ratcliffe blogged his <a href=https://www.socialtext.net/red-herring-spring/index.cgi?mitch_ratcliffe_s_notes>notes from a similar talk</a> that Bill gave at a Red Herring conference.)</p>

<p>You can regard the history of the computer industry as pushing \"dial tone\" further and further up the stack.  As Crichton noted, the rotary dial telephone was the first computer that allowed direct interaction between humans and computers.  The personal computer pushed customer self service up the stack to programming, data processing, and eventually applications such as word processing and spreadsheets.</p>

<p>New applications often start out requiring operators, but eventually move towards dial-tone.  For example, you can look at blogging as the \"dial tone\" equivalent of creating a web site.  For ordinary folks (not most of my readers, but non-technical folks), creating a web site was something that required an operator.  You went to a web design shop or an ISP and had them do it for you.  The blogging revolution, the wiki revolution, the MySpace revolution, the CyWorld revolution, are really about providing a kind of self-service dial-tone for creating a web presence and community.  P2P applications are dial tone for file transfer.  <a href=http://www.sourceforge.net>sourceforge</a> and <a href=http://www.collab.net</a>collab.net</a> are software project hosting dial-tone.  Craigslist is classified advertising dial-tone.</p>

<p>Similarly, you can look at personal databases like Access and Filemaker, and open source databases like MySQL as moving in the direction of providing database dial tone.</p>

<p>Once you frame the problem in this way, you understand that one of the challenges for IT departments and companies used to the IT mindset is to get the operators out of the way, and to build new processes that let users do the work for themselves.  You also can ask yourself, where is dial tone going next?</p>

<p><em>Round 2:  A series of occasional postings around the theme that patterns and ideas recur, or as Arlo Guthrie said in Alice's Restaurant, \"come around again on the gee-tar.\"</em></p>
<br />


<!-- mt:GeoPressMap / -->
<br clear=\"all\" />
</p>
<!-- ISI_LISTEN_STOP -->
<div class=\"tagging\">

<p>
tags:&nbsp;&nbsp;
|&nbsp;<a href=\"http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html#comments\">comments: 9</a> &nbsp;
|&nbsp;<a class=\"iconsphere\" title=\"Related Blogs & Articles\" onclick=\"return Sphere.Widget.search('http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html')\" href=\"http://www.sphere.com/search?q=sphereit:http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\">Sphere It</a>
<br />
submit: <a href=\"http://del.icio.us/post?url=http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\"><img src=\"/images/tag_delicious.gif\" width=\"16\" height=\"16\" /></a> <a href=\"http://reddit.com/submit?url=http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\"><img src=\"/images/tag_reddit.gif\" width=\"20\" height=\"17\" /></a> <a href=\"http://digg.com/submit?phase=2&url=http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\"><img src=\"/images/tag_digg.gif\" width=\"16\" height=\"14\" /></a> <a href=\"http://www.stumbleupon.com/submit?url=http://radar.oreilly.com/archives/2006/08/round-2-dial-tone.html\"><img src=\"/images/tag_stumble.gif\" width=\"16\" height=\"16\" /></a>
</p>

</div> <!-- tagging -->
</div> <!-- content -->
</div> <!-- right -->

<div class=\"clear_float\">&nbsp;</div>
</div>
<div class=\"bot\">
<div class=\"right\"></div>
</div>
</div> <!-- /inner -->
</div>
<!-- /post_block -->

<div class=\"post_nav\">

<a href=\"http://radar.oreilly.com/archives/2006/08/why-use-anything-else.html\" title=\"Why use anything else?\">Previous</a>


&nbsp;|&nbsp;&nbsp;<a href=\"http://radar.oreilly.com/archives/2006/08/paul-yanover-new-head-of-disne.html\" title=\"Paul Yanover new head of Disney Online\">Next</a>

</div>

<h3 class=\"label\" id=\"comments\">Subscribe to Comments on this Entry:</h3>
<div class=\"comment_block\">
<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>
</div>

<div class=\"mid\">
<div class=\"right\">
<div class=\"formBg\">
<form method=\"get\" action=\"http://radar.oreilly.com/mt/mt.cgi\">
<input type=\"hidden\" name=\"__mode\" value=\"verify_subs\" />
<input type=\"hidden\" name=\"entry_id\" value=\"9477\" />
<input id=\"email\" name=\"email\" size=\"16\" value=\"your email address\" />
<input type=\"submit\" class=\"button\" value=\"Go\" />
</form>
</div> <!-- /formBg -->
</div> <!-- /right -->
</div> <!-- /mid -->
<div class=\"bot\">
<div class=\"right\"></div>

</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->




<div id=\"trackbacks\" class=\"trackbacks\">
<h3 class=\"label\">0 TrackBacks</h3>
<div class=\"trackbacks-info\">
<h3 class=\"label\">TrackBack URL for this entry: http://radar.oreilly.com/mt/mt-tb.cgi/8449</h3>
</div>

</div>









<h3 class=\"label\">Comments: 9</h3>


<div class=\"comment_block\">

<div class=\"inner\">

<div class=\"top\">
<div class=\"right\"></div>
</div>
<div class=\"mid\">
<div class=\"right\">
<div class=\"content\">
<p>
<span class=\"author\">
<a title=\"http://www.LyraTechnicalSystems.com\" href=\"http://www.LyraTechnicalSystems.com\" rel=\"nofollow\">Kevin Farnham</a>
</span>
<span class=\"date\">[08.18.06 07:21 AM]</span>
</p>
<p>
<p>
The dial tone concept is interesting. The success of MySpace is attributed by many people to its open structure which allows users to embed their own code into their profiles, blog entries, and the comments they post on other people's profiles. Given the opportunity, people will indeed \"program\" their MySpace sites, including a great many people who otherwise know nothing about software engineering. This has spawned thousands of web sites that provide \"codes\" for non-programmer MySpace users.

</p>
<p>
The next step--one of my goals on MySpace--is to spread awareness that these codes themselves are not too difficult to customize, once you understand the meaning of the characters, once you understand that the words and symbols themselves are actually nothing more than representations of commands and switches, similar to the buttons and links people see on web pages or on their iPods and cell phones. In other words, it's not a huge step from cutting and pasting codes into your MySpace profile to actually looking at the codes, understanding them, modifying/customizing the codes you get from other people, and, finally, creating your own codes--which is programming. Then if you share your code with others, you become a participant in the creation and extension of this human/computer interface \"dial tone.\"
</p>
<p>
Of course, the dial tone is an interface between a human and a computer, but its goal is to facilitate communication and transmission of information between two or more humans, asynchronous communication in the case of blogs and sites like MySpace...
</p>
</p>
</div>
</div>
</div>
<div class=\"bot\">
<div class=\"right\"></div>
</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->







<div class=\"comment_op_block\">


<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>
</div>
<div class=\"mid\">
<div class=\"right\">
<div class=\"content\">

<p>
<span class=\"author\">
<a title=\"http://tim.oreilly.com\" href=\"http://tim.oreilly.com\" rel=\"nofollow\">Tim O'Reilly</a>
</span>
<span class=\"date\">[08.18.06 08:51 AM]</span>
</p>
<p>
<p>Nice comments and information, Kevin.  I'm not as familiar with myspace as I should be, and it's great to have someone reporting on that front of the Web 2.0 expansion.  Feel free to give us more details about interesting news from myspace here.  If it doesn't fit as comments on existing entries, send me ideas directly via email.</p>
</p>
</div>
</div>
</div>
<div class=\"bot\">
<div class=\"right\"></div>

</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->







<div class=\"comment_block\">

<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>

</div>
<div class=\"mid\">
<div class=\"right\">
<div class=\"content\">
<p>
<span class=\"author\">
<a title=\"http://www.chaim.com\" href=\"http://www.chaim.com\" rel=\"nofollow\">Chaim Krause</a>
</span>
<span class=\"date\">[08.18.06 10:48 AM]</span>
</p>
<p>
<p>The problem I have with such a definition is that it begs the question, \"So What?\". Many things have \"create[d] a situation where users can do something for themselves that once required the intervention of an operator\". Why weren't they called Web 2.0?</p>

<p>I believe that Web 2.0 is more than about technology, or, as in the sense used by Tim and Kevin above, the availability and ease of use of that technology to the \"average\" person.</p>

<p>\"So What?\" is a question that helps to discern the portion that, to me, seems to be missing from a purely technical definition.</p>

<p>I can think of two possible answers.</p>

<p>The first answer is that Web 2.0 <em>simplifies life</em> (for a lack of a better term, or maybe repetitive tasks) <i>by providing a central/unified tool</i>. It is easier to use one tool for ten tasks then ten different tools to do the same ten tasks. One can also become more proficient with a tool by using that one tool ten times than they can by using ten different tools only once each.</p>

<p>Also, Web 1.x was limited predominately to information/nouns. You gave data to or received data from Web 1.x. Web 2.0 is predominately action/verb based. People accomplish tasks via Web 2.0. IOW Web 1.x required using (and gaining proficiency with) multiple other tools while Web 2.0 can be used as a (single) tool itself.</p>

<p>Web 1.x stored/delivered documents. Web 2.0 allows for the creation of and collaboration on documents. (Documents could be created without operators before 2.0) Web 1.x stored/delivered photographs. Web 2.0 allows for the creation and  manipulation of photographs, or the ability to transform a photograph into a physical coffee mug or a physical calendar. (Amateur photographers and hobbyist craft-makers didn't require operators.) Web 1.x stored/delivered phone numbers, but Web 2.0 provides a way to place phone calls. (People could place conventional landline or cellphone calls without an operator during Web 1.x.)</p>

<p>So, Web 2.0 combines a)the ability to perform a task (do a verb) and b) the ability to do it all using one unified tool (the web browser/platform) - So What? - to make taking care of reoccurring, necessary, day-to-day tasks easier than it was prior to Web 2.0.</p>

<p>Web 1.0 provided the data that people worked with using disparate tools. A phone number on a web page required a telephone to use. A document stored on an FTP server required a word processing application to use. A data file from your bank's or broker's website required Quicken or MS Money to be of any value.</p>

<p> Web 2.0 provides one multi-purpose tool to make the telephone call (Skype, etc.), collaborate on a document(Writely, etc.), balance your checkbook (an AJAX enabled bank website).</p>

<p>The second answer involves, not the technology, but the people using the technology.</p>

<p>I propose that (in general) Web 2.0 requires the self-service component, but <em>also</em> requires (of course this is only my personal conclusion based on personal observation) the attitude/opinion of the user <em>that they can obtain a <strong>better</strong> result/outcome/product by self-service than they could obtain via a means that was not self-service.</em></p>

<p>Web 2.0 not only allows a person/group to make  100 physical high-quality calendars for their group, but the Web 2.0 <em>user(s)</em> believe(s) they can use Web 2.0 to do it quicker, with less aggravation, simpler, in a word, better, than could be done by another means. IOW by using Web 2.0 to perform the tasks of seeking/obtaining/manipulating possible photos/paper/fonts and discussing/presenting/choosing between themselves the various options, they can obtain/create a <em>better</em> calendar themselves, than can be obtained/created by others using other (non Web 2.0) tools.</p>

<p>An attempt at a summary:</p>
<p>Web 2.0 is defined by</p>
<ul>
<li>its ability to perform actions with/on things rather than simply store/deliver things,</li>
<li>thereby allowing for simplification (of daily life) via a single multi-use tool/platform</li>
<li>that allows a user to gain proficiency more rapidly than via multiple tools/platforms</li>
<li>and users that believe, because of their rapidly-gained high proficiency with Web 2.0, they can create/obtain a better result/product than could be obtained via other means/tools.</li>
</ul>
<p>Please forgive me as this is the first time I have attempted to commit these disparate thoughts to written words.</p>

</p>
</div>
</div>
</div>
<div class=\"bot\">
<div class=\"right\"></div>
</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->






<div class=\"comment_block\">

<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>
</div>
<div class=\"mid\">
<div class=\"right\">
<div class=\"content\">
<p>
<span class=\"author\">
<a title=\"http://www.bluocms.com/bluog\" href=\"http://www.bluocms.com/bluog\" rel=\"nofollow\">Dragos Ilinca</a>
</span>
<span class=\"date\">[08.18.06 12:37 PM]</span>

</p>
<p>
<p>I think we are headed towards something like a shared DIY economy. The revolutionary change that I'm noticing is that we are changing ourselves from being highly specialized in one area to being just literate in hundreds of areas.</p>

<p>This, I think, is the key issue. It's now all about integration rather than division.</p>

<p>In this age, the people that will succeed the most will be 1 or 2% better in hundreds of areas rather than being hundreds of percent better in just one area.</p>

<p>This is now not just possible, but highly probable as the educational entry barrier in using technology is getting lower and lower.</p>
</p>
</div>
</div>
</div>

<div class=\"bot\">
<div class=\"right\"></div>
</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->







<div class=\"comment_block\">

<div class=\"inner\">

<div class=\"top\">
<div class=\"right\"></div>
</div>
<div class=\"mid\">
<div class=\"right\">
<div class=\"content\">
<p>
<span class=\"author\">
<a title=\"http://www.brainbasedbusiness.com\" href=\"http://www.brainbasedbusiness.com\" rel=\"nofollow\">Ellen Weber</a>
</span>
<span class=\"date\">[08.18.06 02:12 PM]</span>
</p>
<p>
<p>Your report of the dial-tone system and metaphor particularly interested me because it is headed toward opening doors for other kinds of thinkers. Some people are better at the IT and others are better at using the technology in very different but equally creative ways. At the moment -- the IT learning is so high that the rest gets lost because one has to spend hours learning and updating IT skills. </p>

<p><a>Brain Based Business</a></p>

<p>Until we move past that stage -- we will only hold back other forms of creativity that could use more technology with less specialty in that angle of their work. Thanks for offering hope for change here! Great discussion and it makes me appreciate the tech specialists even more.</p>
</p>
</div>
</div>
</div>
<div class=\"bot\">
<div class=\"right\"></div>
</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->







<div class=\"comment_block\">

<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>
</div>
<div class=\"mid\">
<div class=\"right\">
<div class=\"content\">
<p>

<span class=\"author\">
<a title=\"http://www.co-creators.co.uk\" href=\"http://www.co-creators.co.uk\" rel=\"nofollow\">Lev</a>
</span>
<span class=\"date\">[08.19.06 05:57 PM]</span>
</p>
<p>
<p>Wow - nice discussion. It raises a lot of aspects. I was very intrigued by Tim's \"dial tone\" metaphor, but I couldn't work out what it was that made me want to comment on it until I read Chaim's \"So what?\" comment. Then it struck me that it was \"So what?\" that was popping up in my head too.</p>

<p>Don't get me wrong, I think Tim's thoughts about the emergence and development of the internet are very insightful and, I agree, the facts and observations you raise are too often overlooked and forgotten.</p>

<p>But if I sit down and think about why that is, I come to the conclusion that it must be based on one thing:</p>

<p>Evolution.</p>

<p>My colleague's son, who is 12 years old, probably couldn't care less about the history of the internet and other open source developments. He was born into a world where the internet/web already existed and he takes it for granted.</p>

<p>There is a disconnect between two generations, the same disconnect between the generations that were around when the telephone went mainstream, or the automobile, or the commercial plan ? I could go on, but you get my point.</p>

<p>I guess what I am trying to say is that we are dealing with a situation where something as significant as the creation and popularisation of the internet was witnessed by one generation, while the next was born into it and has never known a world where the fax machine was the most you could hope for in a modern work environment.</p>

<p>Aren't we simply experiencing a shift where what we learned during the late 80s and 90s has almost become obsolete now, because the next generation doesn't actually need to know how it all started. I had the same thoughts when my friends started making their own websites with a WYSIWYG html editor. After all the work of learning the different HTML tags, my knowledge was almost insignificant because the WYSIWYG html editor allowed anyone to make a website.</p>

<p>Short of being bitter about it, I can see how the new generation will start sticking the finger up at us because while we need to adapt and learn, they simply take Web 2.0 in their stride. So what we are looking at here is a development whereby we allow the next step to be taken more easily because the foundations have been solidly laid.</p>

<p>So my question to you is this ? Is ?dial tone? not just a way of putting a word to the process of evolution?</p>

<p>But to answer your questions as to ?where does dial tone go next?? ? if we knew that then we could jump one stage of evolution, and I don?t know if that is a good thing. One thing is for sure however - the task that lies ahead of IT companies is to try and keep up with the speed that my colleague?s 12 year old son adapts to new developments.</p>
</p>
</div>
</div>
</div>
<div class=\"bot\">
<div class=\"right\"></div>
</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->







<div class=\"comment_block\">

<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>
</div>
<div class=\"mid\">
<div class=\"right\">
<div class=\"content\">
<p>

<span class=\"author\">
<a title=\"http://mikro2nd.net/blog/mike/\" href=\"http://mikro2nd.net/blog/mike/\" rel=\"nofollow\">Mike</a>
</span>
<span class=\"date\">[08.20.06 08:46 AM]</span>
</p>
<p>
<p>Must be humbling - finally stumbling onto what Scott McNeally was telling you what?... 5? 6?.. years ago.  IIRC he was calling it \"webtone\" - a clumsy and dumb name, but anyway - same thing.</p>

<p>I don't imagine for one mikro2nd that Scott though of it himself (though I can probably hazard a couple of good guesses who did think of it).  Nevertheless, Sun is /still/ light years ahead of the rest of us.</p>
</p>
</div>
</div>
</div>

<div class=\"bot\">
<div class=\"right\"></div>
</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->







<div class=\"comment_op_block\">


<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>
</div>
<div class=\"mid\">
<div class=\"right\">
<div class=\"content\">
<p>
<span class=\"author\">
<a title=\"http://tim.oreilly.com\" href=\"http://tim.oreilly.com\" rel=\"nofollow\">Tim O'Reilly</a>
</span>
<span class=\"date\">[08.20.06 09:07 AM]</span>
</p>
<p>
<p>Mike - you're right that Sun (or more specifically, John Gage, who comes up with a lot of these ideas) is light years ahead.  But that being said, I don't think that even though the words are similar, that the ideas are necessarily the same.  </p>

<p>Take Sun's famous \"The network is the computer.\"  It was originally coined to describe a vision of LAN computing, and in fact, when the commercial Internet came along, I spent some time trying to get Sun's marketing folks to expand the way they were using it to put Sun front and center in the internet revolution, but they weren't interested.  (They'd switched to another marketing slogan, I forget which, just at the wrong time.)  But even if they'd used it, I don't think any of us fully understood in the late 80's and early 90's just how true that statement would become.</p>

<p>So when Sun invited me to keynote at JavaOne in 2000, and I gave the talk \"The network <em>really</em> is the computer,\" I used Sun's language but talked about the need for web services (which weren't yet au courant) so we could use web databases as software components and a rediscovery of the Unix pipes and filter mechanism for same (i.e. mashups), which wasn't at all what Sun meant by their original phrase.  They were seeing the future, but that future was richer and more complex than they originally envisioned.  Step by step it becomes clearer.</p>

<p>So it is with webtone.  Go read <a href=\"http://www.sun.com/smi/Press/sunflash/1997-04/sunflash.970415.1113.xml\" rel=\"nofollow\">Sun's original press release on webtone</a>.  You'll see that they define it as giving \"give companies and institutions round-the-clock network dependability for communicating internally and conducting business with suppliers, customers and partners.\"  </p>

<p>Here, the critical element is not dependability or ease of access, it's user self-service.  While there are elements of what I'm talking about here in Sun's concept, I don't think it's the same focus at all, which is no operator intervention.</p>

<p>For example, they would probably not think of Amazon as ecommerce dialtone (as I might), or the consumer uptake of photoshop, photoshop elements, and other digital photography editing tools as kodak dialtone, but those things would be included by the way I'm framing this concept.</p>

<p>But hey, it's not about who said something first.  We all build on each other's ideas, and the fun is <em>using</em> those ideas to see the world in a new way.</p>
</p>
</div>
</div>
</div>
<div class=\"bot\">
<div class=\"right\"></div>
</div>
</div> <!-- /inner -->

</div> <!-- /comment_block -->







<div class=\"comment_block\">

<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>
</div>
<div class=\"mid\">
<div class=\"right\">

<div class=\"content\">
<p>
<span class=\"author\">
SURENDER
</span>
<span class=\"date\">[08.24.07 08:44 PM]</span>
</p>
<p>
<p>abc</p>
</p>
</div>
</div>
</div>
<div class=\"bot\">
<div class=\"right\"></div>

</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->









<h3 class=\"label\">Post A Comment:</h3>
<div class=\"comment_msg\">&nbsp;(please be patient, comments may take awhile to post)</div>

<div class=\"comment_block\">
<div class=\"inner\">
<div class=\"top\">
<div class=\"right\"></div>
</div>
<div class=\"mid\">
<div class=\"right\">
<div class=\"formBg\">

<form method=\"post\" action=\"http://radar.oreilly.com/mt/mt-comments.cgi\" name=\"comments_form\" id=\"comments-form\">
<input type=\"hidden\" name=\"static\" value=\"1\" />
<input type=\"hidden\" name=\"entry_id\" value=\"9477\" />
<input type=\"hidden\" name=\"__lang\" value=\"en\" />

<p>
<label for=\"comment-author\">Name: </label><br />

<input id=\"comment-author\" name=\"author\" size=\"30\" value=\"\" />
</p>
<p>
<label for=\"comment-email\">Email Address: </label><br />
<input id=\"comment-email\" name=\"email\" size=\"30\" value=\"\" />
</p>
<p>
<label for=\"comment-url\">URL: </label><br />
<input id=\"comment-url\" name=\"url\" size=\"30\" value=\"\" />
</p>
<p>
<label for=\"comment-text\">Comments: (You may use HTML tags for style)</label><br />
<textarea id=\"comment-text\" name=\"text\" rows=\"15\" cols=\"50\"></textarea>

</p>


<div class=\"label\"><label for=\"captcha_code\">Captcha:</label></div><div class=\"field\"><input type=\"hidden\" name=\"token\" value=\"enot3EbXitJEhHqTThmoWPhoAonunhLBb0T0BUCa\" /><img src=\"/mt/mt-comments.cgi/captcha/27/enot3EbXitJEhHqTThmoWPhoAonunhLBb0T0BUCa\" width=\"150\" height=\"35\" /><br /><input name=\"captcha_code\" id=\"captcha_code\" value=\"\" autocomplete=\"off\" /><p>Type the characters you see in the picture above.</p></div>
<div id=\"comments-open-captcha\">
</div>


<div id=\"comments-open-footer\">
<!--input type=\"submit\" accesskey=\"v\" name=\"preview\" id=\"comment-preview\" value=\"Preview\" /-->
<input type=\"submit\" accesskey=\"s\" name=\"post\" id=\"comment-submit\" value=\"Submit\" />

</div>
</form>
</div><!-- /formBg -->

</div>
</div>

<div class=\"bot\">
<div class=\"right\"></div>

</div>
</div> <!-- /inner -->
</div> <!-- /comment_block -->








<!-- end alpha-inner content -->

</div>
</div>


<div id=\"beta\">
<div id=\"beta-inner\">

<div class=\"inline\">

<p>
<img src=\"/images/icon_email_rss.png\" width=\"29\" height=\"18\" align=\"left\" alt=\"Subscribe to this Site\" />
<a href=\"feed://radar.oreilly.com/feed\">RSS subscription</a>

<br />
<a href=\"http://radar.oreilly.com/blog-subscription-form.html\">Email subscription</a>
</p>
</div><!-- /inline -->

<!-- include widgets -->










<!--mt:WidgetSet name=\"Main R Col\" /-->
<!-- Special bucket to hold multiple hidden buckets -->
<div id=\"multi_bucket\" class=\"bucket\"> <div class=\"inner\">
<a href=\"javascript:clickMulti(0, 0)\">
<h3 class=\"title\" id=\"multibucket0_0\" style=\"color:#999;\" onmouseover=\"overMulti(0, 0)\" onmouseout=\"offMulti(0 ,0)\">
<a href=\"javascript:clickMulti(0, 0); switchVisible('content0_0_0','content0_0_1','link0_0_0','link0_0_1');\" id=\"link0_0_0\" style=\"color:#666;\">MOST ACTIVE</a> &nbsp;| &nbsp;
<a href=\"javascript:clickMulti(0, 0); switchVisible('content0_0_1','content0_0_0','link0_0_1','link0_0_0');\" id=\"link0_0_1\">MOST RECENT</a>
</h3>
</a>
<div class=\"multi_content\" id=\"content0_0\">
<div class=\"content\" id=\"content0_0_0\">

                                                                                                    <ul>                                                                                        <li><a href=\"http://radar.oreilly.com/archives/2008/05/why-search-competition-isnt-the-point.html\">Why search competition isn't the point</a></li>                                                                                                                                                                                                                                                                                                    <li><a href=\"http://radar.oreilly.com/archives/2008/05/microhoo-corporate-penis-envy.html\">MicroHoo:  corporate penis envy?</a></li>                                                                                                                                                                                                                                                                                                  <li><a href=\"http://radar.oreilly.com/archives/2008/05/the-wiretapping-accusation-aga.html\">The wiretapping accusation against P2P and copyright filtering: evidence that we need more user/provider discussion</a></li>                                                                                                                                                                                                                                                                                                      <li><a href=\"http://radar.oreilly.com/archives/2007/01/openid-get-it-from-yahoo-avoid.html\">OpenID, Get it from Yahoo! & Avoid Phishing</a></li>                                                                                                                                                                                                                                                                                                      <li><a href=\"http://radar.oreilly.com/archives/2008/05/overheard-for-the-twitterverse.html\">Overheard for the Twitterverse</a></li>                                       </ul>
</div>
<div class=\"content\" style=\"display:none;\" id=\"content0_0_1\">

    <ul>
        <li><a href=\"http://radar.oreilly.com/archives/2008/05/why-search-competition-isnt-the-point.html\">Why search competition isn't the point</a></li>


        <li><a href=\"http://radar.oreilly.com/archives/2008/05/the-wiretapping-accusation-aga.html\">The wiretapping accusation against P2P and copyright filtering: evidence that we need more user/provider discussion</a></li>


        <li><a href=\"http://radar.oreilly.com/archives/2008/05/microhoo-corporate-penis-envy.html\">MicroHoo:  corporate penis envy?</a></li>


        <li><a href=\"http://radar.oreilly.com/archives/2008/05/ignite-boston-3-next-week.html\">Ignite Boston 3 - Next week</a></li>


        <li><a href=\"http://radar.oreilly.com/archives/2008/05/overheard-for-the-twitterverse.html\">Overheard for the Twitterverse</a></li>

    </ul>
</div>
</div>

<a href=\"javascript: clickMulti(0, 1)\">
<h3 class=\"title_bot\" id=\"multibucket0_1\" onmouseover=\"overMulti(0, 1)\" onmouseout=\"offMulti(0 ,1)\">RADAR TEAM
</h3>
</a>
<div class=\"content\" id=\"content0_1\" style=\"display:none;\">
<ul>
<li><a href=\"http://radar.oreilly.com/tim/\">Tim O'Reilly</a></li>
<li><a href=\"http://radar.oreilly.com/artur/\">Artur Bergman</a></li>
<li><a href=\"http://radar.oreilly.com/peter/\">Peter Brantley</a></li>

<li><a href=\"http://radar.oreilly.com/dale/\">Dale Dougherty</a></li>
<li><a href=\"http://radar.oreilly.com/brady/\">Brady Forrest</a></li>
<li><a href=\"http://radar.oreilly.com/jimmy/\">Jimmy Guterman</a></li>
<li><a href=\"http://radar.oreilly.com/marc/\">Marc Hedlund</a></li>
<li><a href=\"http://radar.oreilly.com/mikeh/\">Mike Hendrickson</a></li>
<li><a href=\"http://radar.oreilly.com/ben/\">Ben Lorica</a></li>
<li><a href=\"http://radar.oreilly.com/mikel/\">Michael Loukides</a></li>
<li><a href=\"http://radar.oreilly.com/nikolaj/\">Nikolaj Nyholm</a></li>
<li><a href=\"http://radar.oreilly.com/andyo/\">Andy Oram</a></li>

<li><a href=\"http://radar.oreilly.com/allison/\">Allison Randal</a></li>
<li><a href=\"http://radar.oreilly.com/jesse/\">Jesse Robbins</a></li>
<li><a href=\"http://radar.oreilly.com/andrew/\">Andrew Savikas</a></li>
<li><a href=\"http://radar.oreilly.com/nat/\">Nat Torkington</a></li>
<li><a href=\"http://radar.oreilly.com/sara/\">Sara Winge</a></li>
</ul>
</div>
</div>
</div> <!-- /multi_bucket -->

<div class=\"bucket\">

<div class=\"inner\">
<h3 class=\"title\">BUSINESS INTELLIGENCE</h3>
<div class=\"content\">

<div style=\"display: none\" id=\"report1\">
<a href=\"http://radar.oreilly.com/research/where2-report.html\"><img src=\"/images/reports/9780596522551_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Where 2.0: The State of the Geospatial Web\" /></a>
<p>
<a href=\"http://radar.oreilly.com/research/where2-report.html\">Where 2.0: The State of the Geospatial Web</a>
<br />
This report maps out the new generation of geo products and services, identifies the major players, and shows how your business can leverage the power of Where 2.0. <a href=\"http://radar.oreilly.com/research/where2-report.html\">Read more</a>
</p>
<div class=\"clear_float\">&nbsp;</div>
</div>


<div style=\"display: none\" id=\"report2\">

<a href=\"http://radar.oreilly.com/research/facebook-app-eco-report.html\"><img src=\"/images/reports/9780596520076_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Facebook Applications Ecosystem Report\" /></a>
<p>
<a href=\"http://radar.oreilly.com/research/facebook-app-eco-report.html\">The Facebook Application Ecosystem</a>
<br />
What do the killer apps on the Facebook platform have in common? What
must you build into your application if you want Facebook users to
adopt it--and pass it on to their friends? <a href=\"http://radar.oreilly.com/research/facebook-app-eco-report.html\">Read more</a>
</p>
<div class=\"clear_float\">&nbsp;</div>
</div>

<strong><a href=\"http://radar.oreilly.com/research/reports.html\">More Research Reports</a></strong>

</div> <!-- /content -->
</div> <!-- /inner -->

</div> <!-- /bucket -->

<script language=\"JavaScript\" type=\"text/javascript\">
var reportNum = Math.floor(Math.random() * 2) + 1;
var report = $('report' + reportNum);
report.style.display = 'block';
</script>



 <div class=\"bucket\">
<div class=\"inner\">
<h3 class=\"title\">RELEASE 2.0</h3>
<div class=\"content\">
<p><strong>Current Issue</strong></p>
<div>
<a href=\"http://radar.oreilly.com/r2/\"><img src=\"/images/release2_0/9780596520441_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Release 2.0 Current\" /></a>

<p>
<strong><a href=\"http://radar.oreilly.com/r2/\">Money 2.0</a></strong>
<br />
Issue 2.0.8
</p>
<div class=\"clear_float\">&nbsp;</div>
</div>
<div class=\"dashed_line\">&nbsp;</div>
<br />
<p><strong>Back Issues</strong></p>
<div style=\"display: none\" id=\"rel7\">
<a href=\"http://radar.oreilly.com/r2/release2-0-7.html\"><img src=\"/images/release2_0/9780596520403_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Release 2.0.7\" /></a>
<p>
<strong><a href=\"http://radar.oreilly.com/r2/release2-0-7.html\">The Next Generation of CRM</a></strong>

<br />
Issue 2.0.7
</p>
<div class=\"clear_float\">&nbsp;</div>
<br />
</div>
<div style=\"display: none\" id=\"rel6\">
<a href=\"http://radar.oreilly.com/r2/release2-0-6.html\"><img src=\"/images/release2_0/9780596518820_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Release 2.0.6\" /></a>
<p>
<strong><a href=\"http://radar.oreilly.com/r2/release2-0-6.html\">The Hardware Revolution</a></strong>
<br />
Issue 2.0.6
</p>
<div class=\"clear_float\">&nbsp;</div>
<br />
</div>

<div style=\"display: none\" id=\"rel5\">
<a href=\"http://radar.oreilly.com/r2/release2_0_5.html\"><img src=\"/images/release2_0/9780596518059_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Release 2.0.5\" /></a>
<p>
<strong><a href=\"http://radar.oreilly.com/r2/release2_0_5.html\">Looking for the Web's Edge</a></strong>
<br />
Issue 2.0.5
</p>
<div class=\"clear_float\">&nbsp;</div>
<br />
</div>
<div style=\"display: none\" id=\"rel4\">
<a href=\"http://radar.oreilly.com/r2/release2_0_4.html\"><img src=\"/images/release2_0/9780596517045_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Release 2.0.4\" /></a>
<p>
<strong><a href=\"http://radar.oreilly.com/r2/release2_0_4.html\">Information Visualization</a></strong>
<br />

Issue 2.0.4
</p>
<div class=\"clear_float\">&nbsp;</div>
<br />
</div>
<div style=\"display: none\" id=\"rel3\">
<a href=\"http://radar.oreilly.com/r2/release2_0_3.html\"><img src=\"/images/release2_0/9780596516765_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Release 2.0.3\" /></a>
<p>
<strong><a href=\"http://radar.oreilly.com/r2/release2_0_3.html\">Living in Code</a></strong>
<br />
Issue 2.0.3
</p>
<div class=\"clear_float\">&nbsp;</div>
<br />
</div>
<div style=\"display: none\" id=\"rel2\">

<a href=\"http://radar.oreilly.com/r2/release2_0_2.html\"><img src=\"/images/release2_0/9780596516703_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Release 2.0.2\" /></a>
<p>
<strong><a href=\"http://radar.oreilly.com/r2/release2_0_2.html\">Web 2.0 Meets Wall Street</a></strong>
<br />
Issue 2.0.2<br />
<em>Free Download</em>
</p>
<div class=\"clear_float\">&nbsp;</div>
<br />
</div>
<div style=\"display: none\" id=\"rel1\">
<a href=\"http://radar.oreilly.com/r2/release2_0_1.html\"><img src=\"/images/release2_0/9780596516697_sm.gif\" class=\"wrap\" width=\"70\" height=\"90\" alt=\"Release 2.0.1\" /></a>
<p>
<strong><a href=\"http://radar.oreilly.com/r2/release2_0_1.html\">Premier Issue: From 1.0 to 2.0</a></strong>

<br />
Issue 2.0.1
</p>
<div class=\"clear_float\">&nbsp;</div>
<br />
</div>
<strong><a href=\"http://radar.oreilly.com/r2/backissues.html\">More Release 2.0 Back Issues</a></strong>
</div> <!-- /content -->
</div> <!-- /inner -->
</div> <!-- /bucket -->
<script language=\"JavaScript\" type=\"text/javascript\">
    var relNum = Math.floor(Math.random() * 7) + 1;
    var rel = $('rel' + relNum);
    rel.style.display = 'block';
</script>


<div class=\"bucket\">
<div class=\"inner\">
<h3 class=\"title\">CURRENT CONFERENCES</h3>

<div class=\"content\" style=\"display: none\" id=\"conf1\">
<a href=\"http://en.oreilly.com/rails2008/public/content/home\" target=\"_blank\"><img src=\"http://conferences.oreillynet.com/images/rails2007/logo_sm.gif\" class=\"no_wrap\" alt=\"RailsConf 2008\" /></a>
<br />
<p>
Explore the latest Rails developments at this four-day event co-presented by O'Reilly and Ruby Central.
<a href=\"http://en.oreilly.com/rails2008/public/content/home\">Read more</a>
</p>
</div>

<div class=\"content\" style=\"display: none\" id=\"conf2\">

<a href=\"http://en.oreilly.com/velocity2008/public/content/home\" target=\"_blank\"><img src=\"http://conferences.oreillynet.com/images/icons/velocity.gif\" border=\"0\" alt=\"Velocity Logo\" /></a>
<br />
<p>
Velocity is the place to obtain the crucial skills and knowledge to build successful web sites that are fast, scalable, resilient, and highly available.
<a href=\"http://en.oreilly.com/velocity2008/public/content/home\">Read more</a>
</p>
</div>

<div class=\"content\" style=\"display: none\" id=\"conf3\">
<a href=\"http://en.oreilly.com/oscon2008/public/content/home\" target=\"_blank\"><img src=\"http://conferences.oreillynet.com/images/os2007/logo_sm.gif\" border=\"0\" alt=\"Oscon Logo\" /></a>
<br />
<p>
OSCON is the crossroads of all things open source, bringing together
the best, brightest, and most interesting people to explore what's new,
and to champion the cause of open principles and open source adoption
across the computing industry.
<a href=\"http://en.oreilly.com/oscon2008/public/content/home\">Read more</a>
</p>

</div>

</div>
</div> <!-- /bucket -->

<script language=\"JavaScript\" type=\"text/javascript\">
var confNum = Math.floor(Math.random() * 3) + 1;
var conf = $('conf' + confNum);
conf.style.display = 'block';
</script>







<!-- /include widgets -->

</div> <!-- /beta-inner -->
</div> <!-- /beta -->




</div>
</div>
<div id=\"footer\">
<div id=\"footer-inner\">
<div id=\"footer-content\">
<div class=\"inner\">
<p>
<a href=\"http://www.oreilly.com/\">O'Reilly Home</a> | <a href=\"http://www.oreillynet.com/pub/a/mediakit/privacy.html\">Privacy Policy</a> &nbsp; &nbsp; &copy;2005-2008, O'Reilly Media, Inc. &#124; (707) 827-7000 &#047; (800) 998-9938

<br />
Website: <script language=\"JavaScript\" type=\"text/javascript\">
<!--
var name = \"webmaster\";
var domain = \"oreilly.com\";
var display = \"\";
var subject = \"Radar: \";
var ending = \"\";
var style = \"\";
document.write('<a class=\"' + style + '\" href=\"mailto:' + name + '&#64;' + domain + '?subject=' + subject + '\">');
if (display) {
document.write(display);
} else {
document.write(name + '&#64;' + domain);
}
document.write('</a>' + ending);
document.write('<!-- mailmunge bit -->');

// -->
</script>

| Customer Service: <script language=\"JavaScript\" type=\"text/javascript\">
<!--
var name = \"orders\";
var domain = \"oreilly.com\";
var display = \"\";
var subject = \"\";
var ending = \"\";
var style = \"\";
document.write('<a class=\"' + style + '\" href=\"mailto:' + name + '&#64;' + domain + '?subject=' + subject + '\">');
if (display) {
document.write(display);
} else {
document.write(name + '&#64;' + domain);
}
document.write('</a>' + ending);
document.write('<!-- mailmunge bit -->');

// -->
</script>

| Book issues: <script language=\"JavaScript\" type=\"text/javascript\">
<!--
var name = \"booktech\";
var domain = \"oreilly.com\";
var display = \"\";
var subject = \"\";
var ending = \"\";
var style = \"\";
document.write('<a class=\"' + style + '\" href=\"mailto:' + name + '&#64;' + domain + '?subject=' + subject + '\">');
if (display) {
document.write(display);
} else {
document.write(name + '&#64;' + domain);
}
document.write('</a>' + ending);
document.write('<!-- mailmunge bit -->');

// -->
</script>
<br />
All trademarks and registered trademarks appearing on oreilly.com are the property of their respective owners.
</p>
</div>
<!-- Google Analytics tracking code -->

<script type=\"text/javascript\">
var gaJsHost = ((\"https:\" == document.location.protocol) ?
\"https://ssl.\" : \"http://www.\");
document.write(unescape(\"%3Cscript src='\" + gaJsHost +
\"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));
</script>
<script type=\"text/javascript\">
var pageTracker = _gat._getTracker(\"UA-31074-2\");
pageTracker._initData();
pageTracker._trackPageview();
</script>

<!--WEBSIDESTORY CODE HBX1.0 (Universal)-->
<!--COPYRIGHT 1997-2004 WEBSIDESTORY,INC. ALL RIGHTS RESERVED. U.S.PATENT No. 6,393,479B1. MORE INFO:http://websidestory.com / privacy-->
<script language=\"javascript\" type=\"text/javascript\">
var _hbEC=0,_hbE=new Array;function _hbEvent(a,b){b=_hbE[_hbEC++]=new Object();b._N=a;b._C=0;return b;}
var hbx=_hbEvent(\"pv\");hbx.vpc=\"HBX0100u\";hbx.gn=\"ehg-oreilly.hitbox.com\";

hbx.acct=\"DM5505190EVE;DM520404M4ZR\"; //ACCOUNT NUMBER(S)
hbx.pn=\"Tim+OReilly-Round+2:++Dial+Tone;Radar-Tim+OReilly-Round+2:++Dial+Tone\"; //PAGE NAME(S)
hbx.mlc=\"CONTENT+CATEGORY\"; //MULTI-LEVEL CONTENT CATEGORY
hbx.pndef=\"title\"; //DEFAULT PAGE NAME
hbx.ctdef=\"full\"; //DEFAULT CONTENT CATEGORY


hbx.lt=\"auto\"; //LINK TRACKING
hbx.dlf=\"n\"; //DOWNLOAD FILTER
hbx.dft=\"n\"; //DOWNLOAD FILE NAMING
hbx.elf=\"n\"; //EXIT LINK FILTER

//SEGMENTS AND FUNNELS
hbx.seg=\"\"; //VISITOR SEGMENTATION
hbx.fnl=\"\"; //FUNNELS

//CAMPAIGNS
hbx.cmp=\"\"; //CAMPAIGN ID
hbx.cmpn=\"CMP\"; //CAMPAIGN ID IN QUERY
hbx.hra=\"\"; //RESPONSE ATTRIBUTE
hbx.hqsr=\"\"; //RESPONSE ATTRIBUTE IN REFERRAL QUERY
hbx.hqsp=\"ATT\"; //RESPONSE ATTRIBUTE IN QUERY
hbx.hlt=\"\"; //LEAD TRACKING
hbx.hla=\"\"; //LEAD ATTRIBUTE
hbx.gp=\"\"; //CAMPAIGN GOAL
hbx.gpn=\"\"; //CAMPAIGN GOAL IN QUERY
hbx.hcn=\"\"; //CONVERSION ATTRIBUTE
hbx.hcv=\"\"; //CONVERSION VALUE
hbx.cp=\"null\"; //LEGACY CAMPAIGN
hbx.cpd=\"\"; //CAMPAIGN DOMAIN

//CUSTOM VARIABLES
hbx.ci=\"\";//CUSTOMER ID
hbx.hc1=\"\";//CUSTOM 1
hbx.hc2=\"\";//CUSTOM 2
hbx.hc3=\"\";//CUSTOM 3
hbx.hc4=\"\";//CUSTOM 4
hbx.pec=\"\";//ERROR CODES
</script>
<script language=\"javascript1.1\" src=\"http://www.oreillynet.com/hbx.js\" type=\"text/javascript\"></script><!--//-->
<!-- END WEBSIDESTORY CODE -->

<!-- Start Quantcast tag -->
<script type=\"text/javascript\" src=\"http://edge.quantserve.com/quant.js\"></script>
<script type=\"text/javascript\">_qoptions = { tags:\"Radar\" }; _qacct=\"p-20l78bOOCbhcg\";quantserve();</script>

<noscript>
<a href=\"http://www.quantcast.com/p-20l78bOOCbhcg\" target=\"_blank\"><img src=\"http://pixel.quantserve.com/pixel/p-20l78bOOCbhcg.gif?tags=Radar\" style=\"display: none;\" border=\"0\" height=\"1\" width=\"1\" alt=\"Quantcast\"/></a>
</noscript>
<!-- End Quantcast tag -->


</div>
</div>
</div>
</div>
</div>
</div> <!-- /wrapper -->
</div> <!-- /expander -->
</div> <!-- /sizer -->

<script type=\"text/javascript\" id='aptureScript' src=\"http://www.apture.com/js/apture.js?siteToken=o4Af1Db\" charset='utf-8'></script>

</body>
</html>
")

(defparameter *oreilly-prune-test-page*
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<html xmlns=\"http://www.w3.org/1999/xhtml\"><head><title>Round 2:  Dial Tone - O'Reilly Radar</title>
<meta content=\"\" name=\"keywords\"/>
<meta content=\"2006-08-18\" name=\"search_date\"/>
<link type=\"text/css\" href=\"http://radar.oreilly.com/radar.css\" rel=\"stylesheet\"/>
<link href=\"http://radar.oreilly.com/feed\" title=\"Syndication Feed\" type=\"application/atom+xml\" rel=\"alternate\"/>
<link href=\"http://radar.oreilly.com/rsd.xml\" title=\"RSD\" type=\"application/rsd+xml\" rel=\"EditURI\"/>
<link title=\"Why use anything else?\" href=\"http://radar.oreilly.com/archives/2006/08/why-use-anything-else.html\" rel=\"prev\"/>
<script src=\"http://radar.oreilly.com/mt.js\" type=\"text/javascript\"/>
<script src=\"/scripts/lightbox.js\" type=\"text/javascript\"/>
<script src=\"/scripts/prototype.js\" type=\"text/javascript\"/>
</head>
<body onload=\"individualArchivesOnLoad(commenter_name)\" class=\"mt-comment-confirmation layout-wt\">
<div id=\"sizer\">

</div>



</body></html>")

(defvar *imdb-test-title-pages*
  '(("doomsday" . "http://us.imdb.com/title/tt0483607/")
    ("shutter" . "http://us.imdb.com/title/tt0482599/")
    ("iron man" . "http://us.imdb.com/title/tt0371746/")
    ("Indiana Jones and the Kingdom of the Crystal Skull" . "http://us.imdb.com/title/tt0367882/")
    ("What happens in vegas..." . "http://us.imdb.com/title/tt1033643/")
    ("My blueberry nights" . "http://us.imdb.com/title/tt0765120/")
    ("Made of Honor" . "http://us.imdb.com/title/tt0866439/")
    ("Speed racer" . "http://us.imdb.com/title/tt0811080/")))

(defvar *imdb-test-title-search-pages*
  '("http://us.imdb.com/find?q=What+Happens+In+Vegas+(2008);tt=on"
    "http://us.imdb.com/find?q=The+Eye+(2008);tt=on"
    "http://us.imdb.com/find?q=Happy-go-lucky+(2008);tt=on"))

(defvar *imdb-title-page-strings* nil)
(defparameter *imdb-test-page* (cdar *imdb-title-page-strings*))
(defvar *imdb-title-pages* nil)

(defun load-test-imdb-data (&optional (reload t))
  (progn
    (read-site-info "http://us.imdb.com")
    (unless (and (url-site "http://us.imdb.com") (not reload))
      (gather-data
       "http://us.imdb.com"
       "/home/vb/repository/portfolio/crawler/data/seed-urls/imdb.urls"
       #'(lambda (x) (cl-ppcre:all-matches "^http://us.imdb.com/title/tt[0-9]+/$" x))
       :seed-urls (mapcar #'cdr *imdb-test-title-pages*)))
    (setf *imdb-title-page-strings*
          (mapcar #'(lambda (title)
                      (cons (car title)
                            (read-page (cdr title))))
                  *imdb-test-title-pages*))
    (setf *imdb-test-page* (cdar *imdb-title-page-strings*))
    (let ((i -1))
      (setf *imdb-title-pages*
            (mapcar #'(lambda (x)
                        (incf i)
                        (list (car (nth i *imdb-test-title-pages*))
                              (cdr (nth i *imdb-test-title-pages*))
                              (cdr x)))
                    *imdb-title-page-strings*)))
    'done))

(load-test-imdb-data nil)

;(defparameter *test-blocks* (get-tokenized-Blocks *imdb-title-page-strings*))
;(defparameter *test-similar-blocks* (similar-blocks *test-blocks*))

(5am:test simple-test-page-ext
  (5am:is (equal '( "utilitieslink" "jquerycorelink")
                 (ext-html *id-test-page-string* :node :a :class "lin" :att "id")))
  (5am:is (= 12 (length (ext-html *id-test-page-string* :node :a))))
  (5am:is (string=
           (tag-id (car (ext-html *id-test-page-string* :node :a :id "jquerycorelink")))
           "jquerycorelink")))

(5am:test prune-html
  (5am:is (null (ext-html (prune-fluff *id-test-page-string*) :node :style)))
  (5am:is (null (ext-html (prune-fluff *id-test-page-string*) :node :meta)))
  (5am:is (null (ext-html (prune-fluff *id-test-page-string*) :node :script)))
  (5am:is (not (null (ext-html (prune-fluff *id-test-page-string*) :node :div))))
  (5am:is (ext-html *id-test-page-string* :node :div :id "adstorage2"))
  (5am:is (not (ext-html (prune-fluff *id-test-page-string*) :node :div :id "adstorage2"))))

(5am:test page-sig-test
  (5am:is (simple-listp (page-signature
                         *id-test-page*))))


(defparameter *empty-test* "<div class=\"tbcontent\" id=\"root\">


<div class=\"tbcontent\" id=\"nb15\">


 <div class=\"tbcontent\" id=\"nb15botbg\">



  <div id=\"nb15topbg\">
   <div class=\"tbcontent\" id=\"nb15iesux\">

   </div>
  </div>
  <div class=\"tbcontent\" id=\"nb15sub\">

  </div>

 </div>
</div>
</div")

(5am:test empty-test
  (5am:is (tag-empty-p (car (ext-html *empty-test* :id "nb15")))))

#|
(defparameter *test* (analyze-wrappers *imdb-title-pages*))

(defparameter *test-uniqs*
  (uniq-locations (collect-changes *imdb-title-pages*)))

(defparameter *test-non-uniqs*
  (extract-uniqs *test-uniqs* :test #'(lambda (count) (/= count 1))))

(defparameter *test-templates*
  (remove-if-not #'(lambda (x) (member :tbtemplate x))
                 *test* :key #'third))

(5am:test tokenizer
  (5am:is (equal
           (mapcar #'(lambda (page)
             (join
              (tag-tokens
               (first
                (ext-by-loc-struct
                 "html>body>div#wrapper>div#root>div#tn15.maindetails>div#tn15main>div#tn15content>div.strip>table.recs>tbody>tr"
                 '("tr" "td" "a" "td" "a" "td" "a" "td" "a" "td" "a")
                 (third page)))
               :no-atts t)
              " "))
        *imdb-title-pages*)
           '(":tr :td :a 28 Days Later ... :td :a Land of the Dead :td :a Dawn of the Dead :td :a Sin City :td :a Planet Terror"
            ":tr :td :a Shutter :td :a The Ring :td :a The Funhouse :td :a Suspect Zero :td :a The Grudge"
            ":tr :td :a Mission : Impossible III :td :a Transformers :td :a Live Free or Die Hard :td :a 4 : Rise of the Silver Surfer :td :a Commando"
            ":tr :td :a Raiders of the Lost Ark :td :a Indiana Jones and the Temple of Doom :td :a Indiana Jones and the Last Crusade :td :a Pirates of the Caribbean : Dead Man ' s Chest :td :a The Secret of Treasure Island"
            ":tr :td :a 21 :td :a Wedding Crashers :td :a Choses secr tes :td :a Midnight Court :td :a The Notebook"
            ":tr :td :a Maquinista , El :td :a An Unfinished Life :td :a Resurrecting the Champ :td :a Bonnie and Clyde :td :a Intolerable Cruelty"
            ":tr :td :a American Wedding :td :a How to Lose a Guy in 10 Days :td :a Love Actually :td :a Little Manhattan :td :a Everyone Says I Love You"
            ":tr :td :a The Incredibles :td :a Cars :td :a 3 Ninjas :td :a Charlie and the Chocolate Factory :td :a Herbie Fully Loaded")))
  (5am:is (equal
           (length (suffix-array ":tr :td :a 28 Days Later ... :td :a Land of the Dead :td :a Dawn of the Dead :td :a Sin City :td :a Planet Terror"))
           113)))

|#

(defparameter *tag-test*
"  <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\" />
<title>Cineworld Cinemas - Here is what's on today at Cineworld cinemas</title>
<!-- saved from url=(0022)http://internet.e-mail -->

<script language=\"JavaScript\"><!--
function valider(f)
{
 f.submit();
}
function CVF_reset(f)
{
     f.titre.value = \"\";
     f.acteur.value = \"\";
     f.realisateur.value = \"\";
}

function lancerflash (url, largeur, hauteur) {

  var codeflash = '<OBJECT CLASSID=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\"'
   + 'WIDTH=\"' + largeur +' \" HEIGHT=\"' + hauteur + '\"'
   + 'CODEBASE=\"http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab\">'
   + '<PARAM NAME=\"MOVIE\" VALUE=\"' + url + '\">'
   + '<PARAM NAME=\"PLAY\" VALUE=\"true\">'
   + '<PARAM NAME=\"LOOP\" VALUE=\"true\">'
   + '<PARAM NAME=\"QUALITY\" VALUE=\"high\">'
   + '<PARAM NAME=\"MENU\" VALUE=\"false\">'
   + '<EMBED SRC=\"' + url + '\"'
   + 'WIDTH=\"' + largeur +' \" HEIGHT=\"' + hauteur + '\"'
   + 'PLAY=\"true\"'
   + 'LOOP=\"true\"'
   + 'QUALITY=\"high\"'
   + 'MENU=\"false\"'
   + 'TYPE=\"application/x-shockwave-flash\"'
   + 'PLUGINSPAGE=\"http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash\">'
   + '</EMBED>'
   + '</OBJECT>';
   document.write(codeflash);

}

// -->
</script>
<link href=\"/css/style_bc.css\" rel=\"stylesheet\" type=\"text/css\" />
<link href=\"/css/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link href=\"/css/style_cs.css\" rel=\"stylesheet\" type=\"text/css\" />
</head>

<body>
<table width=\"900\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
  <tr>
    <td width=\"70\" rowspan=\"2\" valign=\"top\"><img src=\"/img_new/blank.gif\" width=\"70\" height=\"10\" /></td>
    <td rowspan=\"2\" align=\"left\" valign=\"top\"><table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">

      <tr>
        <td><table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
            <tr>
              <td><table width=\"700\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\">
                  <tr>
                    <td colspan=\"3\" valign=\"top\"><!-- #header -->
<!-- #header_default debut -->
<script language=\"JavaScript\" src='/css/roll.js' type=\"text/javascript\"></script>

<!-- Begin Google Analytics -->
<script type=\"text/javascript\">
var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
document.write(\"\<script src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'>\<\/script>\" );
</script>
<script type=\"text/javascript\">
var pageTracker = _gat._getTracker(\"UA-2971194-1\"); pageTracker._initData(); pageTracker._trackPageview(); </script>
<!-- End Google Analytics -->

<!-- UNSECURE HTTP Tracking -->
<!-- servedby.Pelicanentertainment1513 -->
<!-- servedby.Adserveuk.com1524 -->
<!-- Advivaradius -->
<img height=1 width=1 border=0
src=\"http://servedby.advertising.com/site=703223/mnum=1516/bins=1/rich=0/log
s=0/betr=ae_0010=[+]21|se_0054=[+]1\"
style=\"display:none;\"/>
<!-- tracking end -->


<table width=\"700\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\">
        <tbody>
          <tr>
            <td valign=\"top\"><table border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
                <tr>
                  <td width=\"229\"><a href=\"/\"><img src=\"/picture/Picture.jgi?PICTURE=LOGO\" alt=\"cineworld logo\" border=\"0\" height=\"60\" vspace=\"4\" width=\"229\" /></a></td>
                  <td><iframe src=\"http://adserveuk.com/cid/11421\" name=\"Banner\" width=\"468\" height=\"60\" scrolling=\"No\" frameborder=\"0\" id=\"Banner\"></iframe></td>
                </tr>
            </table></td>
          </tr>
          <tr>
            <td valign=\"top\"><table width=\"700\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" bgcolor=\"#333333\">
                <tr>
                  <td width=\"0\"><img src=\"/img_new/head_h_g.gif\" alt=\"eg\" width=\"24\" height=\"29\" /></td>
                  <td align=\"left\" ><table width=\"100%\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\">
                <tr align=\"center\">
                  <td class=\"hom_menu\"><a href=\"http://www.cineworld.co.uk/\">HOME</a></td>

                  <td class=\"hom_menu\"><a href=\"http://www.cineworld.co.uk/Home.jgi?accueil=+#login\">LOGIN</a></td>
                  <td class=\"hom_menu\"><a href=\"http://www.cineworld.co.uk/Cms.jgi?RUBRIQUE_CMS=UNLIMITED\" >UNLIMITED</a></td>

                  <td class=\"hom_menu\"><a href=\"http://www.cineworld.co.uk/cinematheque/Recommended.jgi\" >RECOMMENDED</a></td>

                  <td class=\"hom_menu\"><a href=\"http://www.cineworld.co.uk/cinematheque/Prochainement.jgi\" >COMING SOON</a></td>

                  <td class=\"hom_menu\"><a href=\"http://www.cineworld.co.uk/Cms.jgi?RUBRIQUE_CMS=SERVICES\" >CINEWORLD SERVICES</a></td>
                    </tr>
                  </table></td>
                  <td align=\"right\"><img src=\"/img_new/head_h_d.gif\" /></td>
                </tr>
            </table></td>
          </tr>
          <tr>
            <td valign=\"top\"><table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
                <tr>
                  <td width=\"176\" align=\"left\"><img src=\"/img_new/head_bloc_g.gif\" alt=\"Find your film\" width=\"176\" height=\"143\" /></td>
            <td valign=\"top\"><table border=\"0\" cellpadding=\"0\" cellspacing=\"0\">
                      <tr>
                        <td colspan=\"6\"><img src=\"/img_new/head_courbe_h.gif\" width=\"524\" height=\"19\" /></td>
                      </tr>
                      <tr>
                        <td width=\"84\" valign=\"top\"><a href=\"http://www.cineworld.co.uk/reservation/ChoixResa.jgi?TYPE=cinema\" ><img src=\"/img_new/head_cinema_uk.gif\" alt=\"Cinemas\" width=\"84\" height=\"89\" border=\"0\" /></a></td>
                        <td width=\"88\" align=\"left\" valign=\"top\"><a href=\"http://www.cineworld.co.uk/cinematheque/Cinematheque.jgi?TYPE=film\" ><img src=\"/img_new/head_films_uk.gif\" alt=\"Films\" width=\"88\" height=\"89\" border=\"0\" /></a></td>
                        <td width=\"88\" valign=\"top\"><a href=\"http://www.cineworld.co.uk/reservation/ChoixResa.jgi?TYPE=horaires\" ><img src=\"/img_new/head_dates_uk.gif\" alt=\"Dates\" width=\"88\" height=\"89\" border=\"0\" /></a></td>
                        <td width=\"88\" valign=\"top\"><a href=\"https://www.cineworld.co.uk/booking/CheckMyBooking.jgi\" ><img src=\"/img_new/head_check_uk.gif\" alt=\"Check my booking\" width=\"88\" height=\"102\" border=\"0\" /></a></td>
                        <td width=\"88\" valign=\"top\"><a href=\"http://www.cineworld.co.uk/Cms.jgi?RUBRIQUE_CMS=GIFTVOUCHE\" ><img src=\"/img_new/head_gift_uk.gif\" alt=\"Gift Vouchers\" width=\"88\" height=\"115\" border=\"0\"/></a></td>
                        <td width=\"88\"><img src=\"/img_new/head_met_opera.gif\" width=\"88\" height=\"124\" border=\"0\"/></td>
                      </tr>
                  </table></td>
                </tr>
            </table></td>
          </tr>
        </tbody>
</table>
<!-- #header_default fin -->
                        <!-- #header_vers_gauche -->                    </td>
                  </tr>
                  <tr>
                    <!-- zone2-->
                    <!-- zone3-->
                    <!-- #gauche -->
<!-- texte_default.html (cinematheque) debut -->
<script language=\"JavaScript\" type=\"text/JavaScript\">
<!--
function MM_swapImgRestore() { //v3.0
  var i,x,a=document.MM_sr; for(i=0;a&&i<a.length&&(x=a[i])&&x.oSrc;i++) x.src=x.oSrc;
}

function MM_preloadImages() { //v3.0
  var d=document; if(d.images){ if(!d.MM_p) d.MM_p=new Array();
    var i,j=d.MM_p.length,a=MM_preloadImages.arguments; for(i=0; i<a.length; i++)
    if (a[i].indexOf(\"#\")!=0){ d.MM_p[j]=new Image; d.MM_p[j++].src=a[i];}}
}

function MM_findObj(n, d) { //v4.01
  var p,i,x;  if(!d) d=document; if((p=n.indexOf(\"?\"))>0&&parent.frames.length) {
    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}
  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];
  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);
  if(!x && d.getElementById) x=d.getElementById(n); return x;
}

function MM_swapImage() { //v3.0
  var i,j=0,x,a=MM_swapImage.arguments; document.MM_sr=new Array; for(i=0;i<(a.length-2);i+=3)
   if ((x=MM_findObj(a[i]))!=null){document.MM_sr[j++]=x; if(!x.oSrc) x.oSrc=x.src; x.src=a[i+2];}
}
//-->
</script>
<body onLoad=\"MM_preloadImages('/img_new/hom_recommended_b_g2.gif','/img_new/hom_unlimited_card_b_g2.gif','/img_new/footer_competition_b_g2.gif')\">

<td width=\"177\" valign=\"top\">
<table width=\"177\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
  <tr>

    <td valign=\"top\" ><table width=\"174\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" bgcolor=\"#38708F\">
        <tr>
          <td colspan=\"2\" align=\"left\" valign=\"top\"> <table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
              <tr>
                <td width=\"17\" valign=\"top\"><img src=\"/img_new/recommended_h.gif\"  width=\"17\" height=\"17\" /></td>
                <td align=\"right\" class=\"hom_bloc_titre marge1\">RECOMMENDED </td>
              </tr>
            </table></td>
          <td rowspan=\"4\" bgcolor=\"#FFFFFF\"><img src=\"/img_new/blank.gif\" width=\"1\" height=\"152\" /></td>
        </tr>
        <tr>
          <td colspan=\"2\" class=\"hom_blocs\">The latest film recommended by Cineworld is:</td>
        </tr>
        <tr>
          <td colspan=\"1\" align=\"left\"  class=\"hom_blocs\" width=\"120\">INDIANA JONES AND THE KINGDOM OF THE CRYSTAL SKULL</td>
          <td colspan=\"1\" align=\"right\"><img src=\"/img_new/star_R3.gif\" /></td>
        </tr>
        <tr>
          <td colspan=\"2\" valign=\"bottom\"><table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
              <tr>
                <td id=\"moreinfo\"><a href=\"/cinematheque/Recommended.jgi\">more info</a></td>
                <td align=\"right\" valign=\"bottom\"><img src=\"/img_new/hom_recommended_b_d.gif\" width=\"22\" height=\"16\" /></td>
              </tr>
            </table></td>
        </tr>
      </table></td>
  </tr>


  <tr>
    <td><img src=\"/img_new/blank.gif\" alt=\"\" width=\"17\" height=\"6\" /></td>
  </tr>
  <tr>

    <td valign=\"top\"><table width=\"174\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" background=\"../img_new/unlimited_fond.jpg\">
        <tr>
          <td width=\"17\" align=\"left\" valign=\"top\"><img src=\"/img_new/footer_left_top.png\" width=\"20\" height=\"20\" controls=\"controls\" border=\"0\" alt=\"x\" class=\"png\" /></td>
          <td class=\"hom_bloc_titre marge1\">UNLIMITED CARD</td>
          <td rowspan=\"4\" bgcolor=\"#FFFFFF\"><img src=\"/img_new/blank.gif\" width=\"1\" height=\"152\" /></td>
        </tr>
        <tr>
          <td colspan=\"2\" class=\"hom_blocs\">Watch as many films as you like from
            just &pound;11.99 per month!</td>
        </tr>
        <tr>
          <td colspan=\"2\" align=\"right\">&nbsp;</td>
        </tr>
        <tr>
          <td colspan=\"2\" valign=\"bottom\"><table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
              <tr>
                <td id=\"moreinfo\"><a href=\"/Cms.jgi?RUBRIQUE_CMS=UNLIMITED\">more info</a></td>
                <td align=\"right\" valign=\"bottom\"><img src=\"/img_new/footer_right-bottom_center.png\" width=\"19\" height=\"20\" border=\"0\" alt=\"x\" class=\"png\" /></td>
              </tr>
            </table></td>
        </tr>
      </table></td>
  </tr>


  <tr>
    <td><img src=\"/img_new/blank.gif\" alt=\"\" width=\"17\" height=\"6\" /></td>
  </tr>

  <tr>
    <td><table width=\"174\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" bgcolor=\"#000000\" style=\"background-image:url(/AfficheCompetitionDyn.jgi?RUBRIQUE_CMS=COMPET&COLONNE_BDD=POSTER_HOME);background-repeat: no-repeat;background-position: top left;\">
        <tr>
          <td align=\"right\"><table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
              <tr>
                <td width=\"20\" valign=\"top\"><img src=\"/img_new/footer_left_top.png\" width=\"20\" height=\"20\" controls=\"controls\" border=\"0\" alt=\"x\" class=\"png\" /></td>
                <td width=\"144\" align=\"right\" class=\"footerTitle1\" nowrap>&nbsp;</td>
                <td width=\"10\" align=\"right\">&nbsp;</td>
              </tr>
          </table></td>
        </tr>
        <tr>
          <td><img src=\"/img_new/blank.gif\" width=\"20\" height=\"89\" /></td>
        </tr>
        <tr>
          <td align=\"left\"><table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
              <tr>
                <td valign=\"middle\" id=\"moreinfo\"><a href=\"/Competition.jgi\">more info</a></td>
                <td width=\"20\" align=\"right\" valign=\"bottom\"><img src=\"/img_new/footer_right-bottom_center.png\" width=\"19\" height=\"20\" border=\"0\" alt=\"x\" class=\"png\" /></td>
              </tr>
          </table></td>
        </tr>
    </table></td>
  </tr>

</table>
</td>

</body>
<!-- texte_default.html (cinematheque) fin-->
                    <!-- #gauche_vers_droite -->
                    <td><img src=\"/img_new/blank.gif\" width=\"4\" height=\"1\" /></td>
                    <!-- #droite -->
<!-- #fiche_film (reservation) debut -->

<script language=\"JavaScript\"><!--
slideshow_a = new Array();
slideshowplace = 0;

hor_a = new Array();
hor_a['out'] = new Image();
hor_a['out'].src = \"/img_new/hor_out.gif\";
hor_a['over'] = new Image();
hor_a['over'].src = \"/img_new/hor_over.gif\";


function popup_args(url,nom,details){
        window.open(url,nom,details);
}

function validerCine(f)
{
 f.submit();
}

function validerDate(f)
{
 f.submit();
}

// -->
</script>

<link rel=\"stylesheet\" href=\"/css/style.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"/css/style_cs.css\" type=\"text/css\" />

  <td valign=\"top\" align=\"left\"><table border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
    <tr>
      <td><table width=\"100%\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\">
          <tr>
            <td align=\"left\" bgcolor=\"#D12300\"><table width=\"100%\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\">
                <tr>
                  <td width=\"13\" align=\"right\" valign=\"middle\"><img src=\"/img_new/blank.gif\" width=\"18\" height=\"30\" /></td>
                  <td align=\"left\"><span class=\"titreblanc\">&nbsp;&nbsp;<b>DIE HARD</b></span></td>
                  <td width=\"18\" align=\"right\" valign=\"top\"><img src=\"/img_new/a_h_d3.gif\" width=\"18\" height=\"19\" /></td>
                </tr>
              </table></td>
          </tr>
          <tr>
            <td><table width=\"100%\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\">
                <tr>
                  <td width=\"5\" bgcolor=\"#D12300\"><img src=\"/img/blank.gif\" width=\"5\" height=\"1\" /></td>
                  <td align=\"left\" valign=\"top\" bgcolor=\"#F8DED9\"><table width=\"100%\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\">

                      <tr>
                        <td width=\"8\" rowspan=\"2\"><img src=\"/img/blank.gif\" /></td>
                        <td rowspan=\"2\" align=\"left\" valign=\"top\"><img src=\"/img/blank.gif\" width=\"1\" height=\"16\" /><br />
                        <img src=\"/img/reservation/no_image.gif\" width=\"93\" height=\"130\" alt=\"DIE HARD - Movie poster\" title=\"DIE HARD\" /></td>
                        <td width=\"14\" height=\"70\"><img src=\"/img/blank.gif\" width=\"1\" height=\"1\" /></td>
                        <td colspan=\"11\" align=\"left\" valign=\"bottom\">
                        </td>
                      </tr>
                      <tr>
                        <td width=\"14\"><img src=\"/img/blank.gif\" width=\"1\" height=\"1\" /></td>
                        <td colspan=\"11\" align=\"left\"><span class=\"basenoir\"><a href=\"http://www.bbfc.co.uk/\" target=\"bbfc\"><img src=\"/img/certificate/18.gif\" alt=\"18\" title=\"18\" border=\"0\" align=\"left\" /><br />
                          </a>&nbsp;<strong></strong>
                          <!-- -->
                        </span> </td>
                      </tr>
                      <tr>
                        <td colspan=\"14\"><img src=\"/img/blank.gif\" height=\"10\" /></td>
                      </tr>
                      <tr>
                        <td width=\"8\"><img src=\"/img/blank.gif\" /></td>
                        <td colspan=\"13\" align=\"left\" valign=\"top\"><table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
                            <tr>
                              <td><span class=\"rbaseNew1\"><b>SYNOPSIS</b></span><br />
                                <span class=\"rbaseNew4\"></span><br />
                              </td>
                              <td width=\"10\">&nbsp;</td>
                            </tr>
                          </table></td>
                      </tr>
                      <tr>
                        <td colspan=\"14\"><img src=\"/img/blank.gif\" width=\"1\" height=\"5\" /></td>
                      </tr>
                      <tr>
                        <td width=\"8\"><img src=\"/img/blank.gif\" /></td>
                        <td valign=\"top\">
                                                  <span class=\"basenoir\"><b>Title:</b></span><br />
                          <span class=\"rbaseNew2\"><b>DIE HARD</b></span><br />
                          <br />
                                                  <span class=\"basenoir\"><b>Director:</b></span><br />
                                                  <span class=\"rbaseNew2\"><b></b></span>
                        </td>
                        <td width=\"14\" ><img src=\"/img/blank.gif\" width=\"10\" /></td>
                        <td colspan=\"7\" valign=\"top\">
                                                  <span class=\"basenoir\"><b>Starring:</b></span><br />
                                                  <span class=\"rbaseNew2\"><b></b></span>
                        </td>

                        <td align=\"left\" valign=\"top\"><img src=\"/img/blank.gif\" width=\"10\" /></td>
                        <td align=\"left\" valign=\"top\">
                                                  <span class=\"basenoir\" style=\"padding-bottom:2px;\"><b>Length:</b><br />
                          </span><span class=\"rbaseNew2\"><b>132min</b></span><br />
                          <br />
                                                  <span class=\"basenoir\"><b>Showing from:</b></span><br />
                          <span class=\"rbaseNew2\"><b> </b></span> </td>
                      </tr>
                      <tr>
                        <td>&nbsp;</td>
                        <td valign=\"top\">&nbsp;</td>
                        <td >&nbsp;</td>
                        <td colspan=\"7\" valign=\"top\"><img src=\"/img/blank.gif\" width=\"150\" height=\"1\" /></td>
                        <td colspan=\"5\" align=\"left\" valign=\"top\"><img src=\"/img/blank.gif\" width=\"100\" height=\"1\" /></td>
                      </tr>
                      <tr>
                        <td colspan=\"14\"></td>
                      </tr>

                      <form name=\"chgdate\" id=\"chgdate\">
                        <input type=\"hidden\" name=\"FILM_ID\" value=\"01936\" />
                        <input type=\"hidden\" name=\"FILMPART_ID\" value=\"\" />
                      </form>

                                          <form action=\"/cinematheque/Cinematheque.jgi?FILM=01936&FILMPART=\" name=\"formulaireDate\" method=\"post\">
                                          </form>

                      <tr>
                        <td width=\"8\"></td>
                        <td colspan=\"13\"><img src=\"/img/blank.gif\" alt=\"\" height=\"1\" width=\"1\" />
                        </td>
                      </tr>
                      <tr>
                        <td colspan=\"14\"><img src=\"/img/blank.gif\" width=\"1\" height=\"5\" /></td>
                      </tr>
                      <tr>
                        <td colspan=\"14\"><img src=\"/img/blank.gif\" width=\"4\" height=\"5\" /></td>
                      </tr>
                      <!--
  <tr>
    <td align=\"right\" valign=\"top\" colspan=\"2\" rowspan=\"3\"></td>
    <td bgcolor=\"#99a3be\" rowspan=\"3\"><img src=\"/img/reservation/bleu_ff.gif\" width=\"1\" height=\"1\"></td>
    <td colspan=\"2\" align=\"right\" valign=\"top\" rowspan=\"3\"></td>
    <td bgcolor=\"#99a3be\" rowspan=\"3\"><img src=\"/img/reservation/bleu_ff.gif\" width=\"1\" height=\"1\"></td>
    <td colspan=\"3\" align=\"right\" valign=\"top\" rowspan=\"3\"><a href=\"javascript:popup('/popup/GestPopup.jgi?TOP=FM&FILM_POPUP=01936&FILMPART=','',775,420)\"><img src=\"/img/reservation/espacemedia_ff.gif\" border=\"0\"></a></td>
    <td bgcolor=\"#99a3be\" rowspan=\"3\"><img src=\"/img/reservation/bleu_ff.gif\" width=\"1\" height=\"1\"></td>
    <td align=\"right\" valign=\"top\" rowspan=\"3\" colspan=\"2\"><a href=\"#\"><img src=\"/img/reservation/distribution_ff.gif\" border=\"0\"></a></td>
    <td bgcolor=\"#99a3be\" rowspan=\"3\"><img src=\"/img/reservation/bleu_ff.gif\" width=\"1\" height=\"1\"></td>
    <td  bgcolor=\"#123a62\" valign=\"top\" rowspan=\"3\"><img src=\"/img/blank.gif\"></td>
    <td  bgcolor=\"#123a62\" valign=\"middle\" ><SPAN class=\"textebase\">livre</SPAN></td>

    <td bgcolor=\"#123a62\"><input type=\"checkbox\"></td>
    <td bgcolor=\"#123a62\" align=\"center\" colspan=\"4\"><SPAN class=\"textebase\"><b>avec</b></SPAN></td>

    <td bgcolor=\"#99a3be\"  rowspan=\"3\"><img src=\"/img/reservation/bleu_ff.gif\" width=\"1\" height=\"1\"></td>
    <td bgcolor=\"#123a62\" align=\"center\"><SPAN class=\"textebase\"><b>Goodies</b></SPAN></td>
    <td bgcolor=\"#123a62\" valign=\"top\" align=\"right\" ><input type=\"checkbox\"></td>
    <td bgcolor=\"#99a3be\" rowspan=\"3\"><img src=\"/img/reservation/bleu_ff.gif\" width=\"1\" height=\"1\"></td>
  </tr>
  -->

                    </table></td>
                  <td width=\"5\"  bgcolor=\"#D12300\"><img src=\"/img/blank.gif\" width=\"5\" height=\"1\" /></td>
                </tr>
              </table></td>
          </tr>
          <tr align=\"left\">
            <td valign=\"bottom\" bgcolor=\"#D12300\"> <table width=\"100%\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\">
                <tr>
                  <td width=\"18\" align=\"left\" valign=\"bottom\"><img src=\"/img_new/a_b_g3.gif\" width=\"18\" height=\"19\" /></td>
                  <td colspan=\"6\" align=\"left\">&nbsp;</td>
                  <td colspan=\"9\" align=\"right\">&nbsp;</td>
                </tr>
              </table></td>
          </tr>
        </table></td>
        </tr>
</table>

</td>

<!-- #fiche_film (reservation) fin -->
                    <!-- #droite_vers_footer -->
                    <!-- #bas -->
                  </tr>
              </table></td>
            </tr>
        </table></td>
      </tr>
      <tr>
        <td colspan=\"3\" align=\"center\" valign=\"top\"><table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" id=\"footer\">
            <tbody>
              <tr>
                <td colspan=\"19\" bgcolor=\"#ffffff\"><img src=\"/img_new/bl.gif\" alt=\"\" height=\"1\" width=\"1\" /></td>
              </tr>
              <tr align=\"center\">
                      <td><a href=\"/\" >Home</a></td>
                      <td ><span class=\"lienns\"><img src=\"/img_new/blank.gif\" alt=\"\" height=\"3\" width=\"3\" /></span></td>
                      <td align=\"center\"><a href=\"/Cms.jgi?RUBRIQUE_CMS=CAREERS\" >Careers</a></td>
                      <td><span class=\"lienns\"><img src=\"/img_new/blank.gif\" alt=\"\" height=\"3\" width=\"3\" /></span></td>
                      <td align=\"center\" ><a href=\"/Cms.jgi?RUBRIQUE_CMS=TERMS\" >Terms of Use</a></td>
                      <td class=\"lienns\"><img src=\"/img_new/blank.gif\" alt=\"\" height=\"3\" width=\"3\" /></td>
                      <td align=\"center\" ><a href=\"/Cms.jgi?RUBRIQUE_CMS=POLICY\" >Privacy Policy </a></td>
                      <td><span class=\"lienns\"><img src=\"/img_new/blank.gif\" alt=\"\" height=\"3\" width=\"3\" /></span></td>
                      <td align=\"center\"><a href=\"/Cms.jgi?RUBRIQUE_CMS=FAQ\" >FAQ</a></td>
                      <td align=\"center\" ><span class=\"lienns\"><img src=\"/img_new/blank.gif\" alt=\"\" height=\"3\" width=\"3\" /></span></td>
                      <td align=\"center\" ><a href=\"/Cms.jgi?RUBRIQUE_CMS=CONTACTUS\" >Contact Us</a></td>
                      <!--
                      <td><span class=\"lienns\"><img src=\"/img_new/blank.gif\" alt=\"\" height=\"3\" width=\"3\" /></span></td>
                      <td align=\"center\"><a href=\"/Cms.jgi?RUBRIQUE_CMS=HELP\" >Help</a></td>
                      -->
                      <td><span class=\"lienns\"><img src=\"/img_new/blank.gif\" alt=\"\" height=\"3\" width=\"3\" /></span></td>
                      <td align=\"center\" ><a href=\"/sitemap\" >Site Map</a></td>

                          <td class=\"lienns\"><img src=\"/img_new/blank.gif\" alt=\"\" height=\"3\" width=\"3\" /></td>
                          <td align=\"center\" ><a href=\"http://www.cineworldplc.com/\" onclick=\"window.open('http://www.cineworldplc.com/','suggestions','toolbar=0,location=0,directories=0,status=0,scrollbars=1,resizable=1,copyhistory=0,menuBar=0,width=760,height=700');return(false)\" >Cineworld Plc</a></td>

                      <td><span class=\"lienns\"><img src=\"/img_new/blank.gif\" alt=\"\" height=\"3\" width=\"3\" /></span></td>
                      <td align=\"center\" ><a href=\"/Cms.jgi?RUBRIQUE_CMS=DISABLED\" >Disabled Access</a></td>

              </tr>
              <tr>
                <td colspan=\"19\" bgcolor=\"#ffffff\"><img src=\"/img_new/bl.gif\" alt=\"\" height=\"1\" width=\"1\" /></td>
              </tr>
              <tr>
                <td colspan=\"19\"><img src=\"/img_new/blank_002.gif\" alt=\"\" height=\"20\" width=\"1\" /></td>
              </tr>
            </tbody>
        </table></td>
      </tr>
    </table></td>
    <td rowspan=\"2\" valign=\"top\"><img src=\"/img_new/blank.gif\" width=\"10\" height=\"10\" /></td>
    <td width=\"120\" height=\"4\" valign=\"top\"><img src=\"/img_new/blank.gif\" width=\"10\" height=\"4\" /></td>
  </tr>
  <tr>
    <td valign=\"top\" width=\"120\"><iframe src=\"http://adserveuk.com/cid/11422\" name=\"Sky\" width=\"120\"
height=\"600\" scrolling=\"No\" frameborder=\"0\" id=\"Sky\"></iframe></td>
  </tr>
</table>
</body>
</html>

")
