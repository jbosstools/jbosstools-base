<%@page contentType="text/html;charset=UTF-8"%>
<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core"%>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html"%>
<%@ taglib prefix="s" uri="http://struts.apache.org/shale/core"%>
<%@ taglib prefix="clay"
	uri="http://struts.apache.org/shale/clay-plugin"%>
<html>
<head>
<title>Rolodex Example Using Clay</title>

<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<style type="text/css" media="screen">
body {
	margin : 10px;
	font: Verdana, Helvetica, Arial;
	padding: 0px;
	background: #fff;
}

#menu {
	border-bottom : 1px solid #ccc;
	margin : 0;
	padding-bottom : 19px;
	padding-left : 10px;
}

#menu ul, #menu li	{
	display : inline;
	list-style-type : none;
	margin : 0;
	padding : 0;
}

	
#menu a:link, #menu a:visited	{
	background : #E8EBF0;
	border : 1px solid #ccc;
	color : #666;
	float : left;
	font-size : small;
	font-weight : normal;
	line-height : 14px;
	margin-right : 8px;
	padding : 2px 10px 2px 10px;
	text-decoration : none;
}

#menu a:link.active, #menu a:visited.active	{
	background : #fff;
	border-bottom : 1px solid #fff;
	color : #000;
}

#menu a:hover	{
	color : #f00;
}

.contacts {
   border: 1px solid #fff;
}

.contactsHeader {
   text-align: center;
   color: #99CC66;
   background: #E5F2D9;   
}

.contactsRow2 {
   text-align: left;
   color: #99CC66;
   background: #D9EEF2;   
}

.contactsRow1 {
   text-align: left;
   color: #99CC66;
   background: #F1F7D4;   
}
	
body.section-1 #menu li#nav-sel a {
	background : #fff;
	border-bottom : 1px solid #fff;
	color : #000;
}

li#nav {
	background : #red;
	border-bottom : 1px solid #000;
	color : #000;
}

#menu ul a:hover {
	color : #f00 !important;
}

#contents {
	background : #fff;
	border : 1px solid #ccc;
	border-top : none;
	clear : both;
	margin : 0px;
	padding : 15px;
}

input.button {
    width: 1.2in; 
    text-align: center; 
    color: #99CC66; 
    font-size: 12px; 
    font-style: normal; 
    font-weight: bold
}

</style>


</head>

<f:view>
<%@include file="../messages.jspf"%>
	<body class="section-1">

	<h:form>
        <h:commandLink action="home" value="Back" immediate="true"/><br/><br/>

		<!-- Example of the runtime option.  The createTabs method binding event builds the tab links 
            dynamically in the view controller -->

		<clay:clay id="tabs" jsfid="RUNTIME"
			shapeValidator="#{rolodex.createTabs}" managedBeanName="rolodex" />

		<!-- Example of the xml composition option. -->
		<div id="contents"><h:panelGrid columns="2">

			<!-- A list of contacts to select from -->
			<clay:clay id="contacts" jsfid="contactTable"
				managedBeanName="rolodex" />

			<h:panelGrid columns="2">
				<clay:clay id="namePanel" jsfid="namePanel"
					managedBeanName="rolodex.selectedContact" />
				<clay:clay id="commandPanel" jsfid="commandPanel"
					managedBeanName="rolodex" />
				<h:outputText value="#{messages['rolodex.address.residentialAddress']}" style="color:#66B9CC"/>
				<h:outputText value="#{messages['rolodex.address.businessAddress']}" style="color:#66B9CC"/>
				<!-- clay:clay id="address1" jsfid="addressPanel"
					managedBeanName="rolodex.selectedContact.residentialAddress" / -->
				<clay:clay id="address1" jsfid="rolodex/address.html"
					managedBeanName="rolodex.selectedContact.residentialAddress" />
				<clay:clay id="address2" jsfid="foreignAddressPanel"
					managedBeanName="rolodex.selectedContact.businessAddress" />
				<clay:clay id="phone1" jsfid="residentialPhonePanel"
					managedBeanName="rolodex.selectedContact" />
				<clay:clay id="phone2" jsfid="businessPhonePanel"
					managedBeanName="rolodex.selectedContact" />
			</h:panelGrid>
		</h:panelGrid></div>
	</h:form>
	</body>
</f:view>
</html>

