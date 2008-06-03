<%@ taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<%@ taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@ taglib uri="http://myfaces.apache.org/tomahawk" prefix="t"%>
<f:loadBundle basename="demo.bundle.Messages" var="Message"/>

<HTML>
<HEAD> <title>Greeting Page</title> </HEAD>

<body bgcolor="white">
	<f:view>
		<h3>
    		<t:outputText value="#{Message.greeting_text}"/>
    		<t:outputText value="#{GetNameBean.userName}"/>
		</h3>
	</f:view>
</body>
</HTML>