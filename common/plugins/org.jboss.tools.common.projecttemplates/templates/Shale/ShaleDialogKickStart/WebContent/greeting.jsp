<%@page contentType="text/html;charset=UTF-8"%>

<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="s" uri="http://struts.apache.org/shale/core" %>

<f:view>
	<f:loadBundle var="messages" basename="dialog.Bundle"/>
	<html>
	<head>
		<title>
			<h:outputText value="#{messages['title']}"/>
		</title>
	</head>
	<body>
		<h:form id="dialogForm">
			<h1>
				<h:outputText value="Welcome #{welcome.name}!" />
			</h1>
			<h:panelGrid columns="1">
				<h:commandLink id="welcomeDialog" action="dialog:Welcome" rendered="true">
					<h:outputText value="#{messages['input.name']}"/>
				</h:commandLink>
			</h:panelGrid>
		</h:form>
	</body>
	</html>
</f:view>