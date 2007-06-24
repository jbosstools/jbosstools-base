<%@page contentType="text/html;charset=UTF-8"%>

<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core"%>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html"%>
<%@ taglib prefix="s" uri="http://struts.apache.org/shale/core"%>

<f:view>
	<f:loadBundle var="messages" basename="dialog.Bundle" />
	<html>
		<head>
		<title><h:outputText value="#{messages['input.title']}" /></title>
		</head>
		<body>
			<h:form id="welcomeForm">
				<h:outputLabel for="username">
					<h:outputText value="#{messages['prompt.username']}" />
				</h:outputLabel>
				<h:inputText id="username" required="true" value="#{welcome.inputName}" />
				<h:commandButton id="ok" action="ok" rendered="true" value="#{messages['button.ok']}"/>
				<h:commandButton id="cancel" action="cancel" rendered="true" value="#{messages['button.cancel']}"/>
			</h:form>
		</body>
	</html>
</f:view>