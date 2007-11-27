<%@ taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<%@ taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@ taglib uri="http://myfaces.apache.org/tomahawk" prefix="t"%>
<f:loadBundle basename="demo.bundle.Messages" var="Message"/>

<HTML>
    <HEAD> <title>Input Name Page</title> </HEAD>
    <body bgcolor="white">
	<f:view>
		<h1><t:outputText value="#{Message.inputname_header}"/></h1>
		<t:messages style="color: red"/>
    	<h:form id="helloForm">
    		<t:outputText value="#{Message.prompt}"/>
			<t:inputText value="#{GetNameBean.userName}">
	    		<f:validateLength minimum="2" maximum="20"/>
    		</t:inputText>
	 	<t:commandButton id="submit" action="sayhello" value="Say Hello" />
    	</h:form>
	</f:view>
    </body>
</HTML>