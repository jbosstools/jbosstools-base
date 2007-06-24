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
  <h:outputText        value="#{messages['validate.test.thank.you.title']}"/>
</title>
</head>
<body>

  <h:form                 id="form"> 

    <h:outputText      value="#{messages['validate.test.payment.thank.you.heading']}"
                       style="font-size: 1.2em;"/>
    <p>
    <h:panelGrid          id="grid"
                     columns="3">

      <h:outputLabel     for="amount"    
                       value="#{messages['validate.test.amount']}"/>

      <h:outputText    value="#{messages['validate.test.currency.symbol']}"/>

      <h:outputText       id="amount"
                       value="#{validate$test.amount}">

            <f:convertNumber 
           minFractionDigits="2"/> 

      </h:outputText>


      <h:outputLabel     for="creditCard"    
                       value="#{messages['prompt.creditCardNumber']}"/>

      <h:outputText    value=""/>

      <h:outputText       id="creditCard"
                       value="#{validate$test.creditCard}">

      </h:outputText>

      <h:outputText    value="#{messages['prompt.expirationDate']}"/>

      <h:outputText    value=""/>

      <h:outputText       id="date"
                       value="#{validate$test.expirationDate}">

          <f:convertDateTime 
                     pattern="MM/dd/yyyy"/>

       </h:outputText>
     
    </h:panelGrid>
      
    <h:panelGrid columns="1">
       <h:commandButton   
                       value="Back"
                      action="validate$test"/>

       <f:verbatim><p></f:verbatim>

       <h:commandLink   
                       value="Use cases top-level menu"
                      action="usecases$toplevel"/>
    </h:panelGrid>

    <s:validatorScript functionName="validateForm"/>

  </h:form>

</body>
</html>
</f:view>
