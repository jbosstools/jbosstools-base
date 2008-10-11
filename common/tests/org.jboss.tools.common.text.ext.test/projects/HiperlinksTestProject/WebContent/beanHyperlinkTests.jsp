<%@ taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@ taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<jsp:useBean id="b1" class="org.jboss.tools.test.TestBean1"></jsp:useBean>
<jsp:getProperty property="property1" name="b1"/>

