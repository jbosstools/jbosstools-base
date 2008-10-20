<%@ taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@ taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<jsp:useBean id="b1" class="org.jboss.tools.test.TestBean1"></jsp:useBean>
<jsp:getProperty property="property1" name="b1"/>
<jsp:setProperty property="property2" name="b1"/>
<html>
<body>	
	<a href="classHyperlinkTests.jsp">${b1.property2}</a>
</body>
</html>

