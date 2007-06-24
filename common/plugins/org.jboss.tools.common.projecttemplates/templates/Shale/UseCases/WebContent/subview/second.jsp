<%@page contentType="text/html;charset=UTF-8"%>
<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="s" uri="http://struts.apache.org/shale/core" %>

<%--

 Copyright 2004-2005 The Apache Software Foundation.
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 
      http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

--%>

<f:view>
<%@include              file="../messages.jspf"%>
<html>
<head>
<title>
  <h:outputText        value="#{messages['subview.second.title']}"/>
</title>
</head>
<body>

  <h:form                 id="form">

    <h:panelGrid     columns="1">

      <s:subview          id="subview_gamma">
        <jsp:include    page="/subview/gamma.jsp"/>
      </s:subview>

      <s:subview          id="subview_delta">
        <jsp:include    page="/subview/delta.jsp"/>
      </s:subview>

      <h:panelGroup>
        <h:outputText  value="#{messages['subview.expected']}"/>
        <h:outputText     id="expected"
                       value="#{subview$second.expected}"/>
      </h:panelGroup>

      <h:panelGroup>
        <h:outputText  value="#{messages['subview.actual']}"/>
        <h:outputText     id="actual"
                       value="#{actual}"/>
      </h:panelGroup>

      <%-- Submits back to this page --%>
      <h:commandButton    id="continue"
                        type="submit"
                       value="#{messages['subview.continue']}"/>

      <h:commandButton    id="finish"
                      action="subview$third"
                        type="submit"
                       value="#{messages['subview.finish']}"/>

    </h:panelGrid>

  </h:form>


</body>
</html>
</f:view>
