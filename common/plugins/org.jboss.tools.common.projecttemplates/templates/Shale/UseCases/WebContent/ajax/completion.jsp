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
  <h:outputText        value="#{messages['ajax.completion.title']}"/>
</title>
<script                 type="text/javascript">

  // Global variables initialized at page load time
  var name;                  // Element for the auto-completion field (name)
  var table;                 // Table element containing the available names

  // Global variables initialized per keystroke
  var isIE;                  // Set to "true" for IE
  var request;               // Platform dependent XMLHttpRequest instance

  // append() - Append a state abbreviation and name to the table
  function append(abbreviation, name) {
    if (isIE) {
      row = table.insertRow(table.rows.length);
    } else {
      row = document.createElement("tr");
      table.appendChild(row);
    }
    col = document.createElement("td");
    col.appendChild(document.createTextNode(abbreviation));
    row.appendChild(col);
    col = document.createElement("td");
    col.appendChild(document.createTextNode(name));
    row.appendChild(col);
  }

  // clear() - Clear the available names table
  function clear() {
    // Clear all the children (rows) of the table
    for (i = table.childNodes.length - 1; i >= 0; i--) {
      table.removeChild(table.childNodes[i]);
    }
    // Add back the column headers
    if (isIE) {
      row = table.insertRow(table.rows.length);
    } else {
      row = document.createElement("tr");
      table.appendChild(row);
    }
    head = document.createElement("th");
    head.appendChild(document.createTextNode("Abbr"));
    row.appendChild(head);
    head = document.createElement("th");
    head.appendChild(document.createTextNode("State Name"));
    row.appendChild(head);
  }

  // complete() - Perform completion processing on each keystroke
  function complete() {
    alert("complete()");
    var url = "list/stateNames.remote?prefix=" + escape(name.value);
    setup();
    request.onreadystatechange = process;
    request.open("GET", url, true);
    request.send(null);
  }

  // init() - Initialize variables on page load
  function init() {
    name = document.getElementById("form:name");
    table = document.getElementById("table");
  }

  // parse() - Parse the response and repopulate the available options table
  function parse() {
    clear();
    states = request.responseXML.getElementsByTagName("items")[0];
    for (i = 0; i < states.childNodes.length; i++) {
      abbreviation = states.getElementsByTagName("value")[0];
      name = states.getElementsByTagName("label")[0];
      append(abbreviation.childNodes[0].nodeValue, name.childNodes[0].nodeValue);
    }
  }

  // process() - Callback function to handle asynchronous response
  function process() {
    if (request.readyState == 4) {
      if (request.status == 200) {
        parse();
      } else if (request.status == 204) {
        alert("Clearing the table");
        clear();
      }
    }
  }

  // setup() - Set up XMLHttpRequest object and related variables
  function setup() {
    if (window.XMLHttpRequest) {
      isIE = false;
      request = new XMLHttpRequest();
    } else if (window.ActiveXObject) {
      isIE = true;
      request = new ActiveXObject("Microsoft.XMLHTTP");
    }
  }

</script>
</head>
<body                 onload="init()">

  <h:form                 id="form">

    <h:panelGrid          id="grid"
                     columns="3">

      <h:outputText    value="#{messages['ajax.completion.prompt']}"/>
      <h:inputText        id="name"
                     onkeyup="complete();"
                       value="#{ajax$completion.name}"/>
      <f:verbatim>
        <table            id="table"
                      border="0">
          <tr             id="header">
            <th>Abbr</th>
            <th>State Name</th>
          </tr>
        </table>
      </f:verbatim>

      <h:outputText       id="result"
                       value="#{ajax$completion.result}"/>
      <h:commandButton    id="submit"
                       value="#{messages['ajax.completion.submit']}"
                      action="#{ajax$completion.submit}"/>
      <h:commandButton    id="finish"
                       value="#{messages['ajax.completion.finish']}"
                      action="exit"/>

      <h:messages         id="messages"
                  globalOnly="false"/>

    </h:panelGrid>

  </h:form>

</body>
</html>
</f:view>
