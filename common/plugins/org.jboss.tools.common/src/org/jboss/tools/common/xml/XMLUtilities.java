/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.xml;

import java.io.*;
import java.util.*;
import javax.xml.parsers.DocumentBuilder;
import org.apache.xml.serialize.*;
import org.w3c.dom.*;
import org.xml.sax.*;

public class XMLUtilities {
    public static boolean hasAttribute(Element e, String s) {
        return e.getAttributes().getNamedItem(s) != null;
    }

    public static Element[] getChildren(Element parent, String name){
        ArrayList<Element> al = new ArrayList<Element>();
        NodeList nl = parent.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++){
            Node n = nl.item(i);
            if(n.getNodeType() == Node.ELEMENT_NODE &&
               n.getNodeName().equals(name)) {
                al.add((Element)n);
            }
        }
        return al.toArray(new Element[0]);
    }

    public static Element getUniqueChild(Element parent, String name){
        Element[] e = getChildren(parent, name);
        return (e.length == 0) ? null : e[0];
    }

    public static Element getFirstChild(Element parent, String name) {
        NodeList nl = parent.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++){
            Node n = nl.item(i);
            if(n.getNodeType() == Node.ELEMENT_NODE &&
               n.getNodeName().equals(name)) {
                return (Element)n;
            }
        }
        return null;
    }

    public static Element getLastChild(Element parent, String name) {
    	Element child = null;
        NodeList nl = parent.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++){
            Node n = nl.item(i);
            if(n.getNodeType() == Node.ELEMENT_NODE &&
               n.getNodeName().equals(name)) {
               child = (Element)n;
            }
        }
        return child;
    }

    public static Element[] getAncestors(Element parent, String name) {
        int i = name.indexOf('.');
        if(i < 0) return getChildren(parent, name);
        Element p = getUniqueChild(parent, name.substring(0, i));
        return (p == null) ? new Element[0] : getAncestors(p, name.substring(i + 1));
    }

    public static Element createElement(Element parent, String path) {
        int i = path.indexOf('.');
        if(i < 0) {
            Element element = parent.getOwnerDocument().createElement(path);
            parent.appendChild(element);
            return element;
        }
        String p = path.substring(0, i), c = path.substring(i + 1);
        Element pe = getUniqueChild(parent, p);
        if(pe == null) {
            pe = parent.getOwnerDocument().createElement(p);
            parent.appendChild(pe);
        }
        return createElement(pe, c);
    }

	public static DocumentBuilder createDocumentBuilder() {
		return createDocumentBuilder(false);
	}

	public static DocumentBuilder createDocumentBuilder(boolean validating) {
		return SafeDocumentBuilderFactory.createDocumentBuilder(validating);
	}

    public static Element createDocumentElement(String name) {
		Document d = createDocumentBuilder().newDocument();
        Element de = d.createElement(name);
        d.appendChild(de);
        return de;
    }

    public static Element createDocumentElement(String name, String qName, String publicId, String systemId, String namespaceURI) {
        Document d = null;
        try {
            DOMImplementation domImpl = createDocumentBuilder().getDOMImplementation();
            DocumentType docType = domImpl.createDocumentType(qName, publicId, systemId);
            d = domImpl.createDocument(namespaceURI, name, docType);
        } catch (Exception e) {
            return null;
        }
        Element de = d.getDocumentElement();
        if (de == null) {
            de = d.createElement(name);
            d.appendChild(de);
        }
        return de;
    }

    public static Element getElement(String filename, EntityResolver resolver) {
        try {
            return getElement(new File(filename), resolver);
        } catch (Exception e) {
            return null;
        }
    }

    public static Element getElement(File file, EntityResolver resolver) {
        java.io.FileReader fr = null;
        try {
            fr = new java.io.FileReader(file);
            org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(fr);
            return getElement(inSource, resolver);
        } catch (Exception e) {
            return null;
        } finally {
            try {
                if (fr != null) fr.close();
            } catch (Exception e) {}
        }
    }

    public static Element getElement(Reader reader, EntityResolver resolver) {
        Document d = getDocument(reader, resolver);
        return (d == null) ? null : d.getDocumentElement();
    }

    public static Document getDocument(Reader reader, EntityResolver resolver) {
        try {
            org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(reader);
            return getDocument(inSource, resolver);
        } catch (Exception e) {
            return null;
        }
    }

    public static Element getElement(InputStream is, EntityResolver resolver) {
        try {
            org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(is);
            return getElement(inSource, resolver);
        } catch (Exception e) {
            return null;
        }
    }

    public static Element getElement(InputSource is, EntityResolver resolver) throws Exception {
        return getDocument(is, resolver).getDocumentElement();
    }
    
    public static Document getDocument(InputSource is, EntityResolver resolver) throws Exception {
		DocumentBuilder builder = createDocumentBuilder(false);
		if(resolver != null) builder.setEntityResolver(resolver);
		return builder.parse(is);
    }

	public static String[] getXMLErrors(Reader reader, EntityResolver resolver) {
		return getXMLErrors(reader, true, resolver);
	}
    
    public static String[] getXMLErrors(Reader reader, boolean checkDTD, EntityResolver resolver) {
        try {
            org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(reader);
            return getXMLErrors(inSource, checkDTD, resolver);
        } catch (Exception e) {
            return new String[]{e.getMessage()};
        }
    }
    
	public static String[] getXMLErrors(org.xml.sax.InputSource is, EntityResolver resolver) {
		return getXMLErrors(is, true, resolver);
	}
		
    public static String[] getXMLErrors(org.xml.sax.InputSource is, boolean checkDTD, EntityResolver resolver) {
		ErrorHandlerImpl h = new ErrorHandlerImpl();
        try {
			DocumentBuilder builder = createDocumentBuilder(checkDTD);
            if(resolver != null) builder.setEntityResolver(resolver);
            builder.setErrorHandler(h);
            builder.parse(is);
        } catch (Exception e) {
        	if(h.errors.isEmpty()) return new String[]{"Unidentified parser error:0:0"};
        }
        return h.errors.toArray(new String[0]);        
    }
    
    public static final void serialize(Element element, String filename) throws IOException {
        File f = new File(filename);
        if(f.exists() && !f.canWrite()) return;
        if(!f.exists()) f.createNewFile();
        FileWriter fw = new FileWriter(f);
        serialize(element, new BufferedWriter(fw));
        fw.close();
    }

	public static String getEncoding(String body) {
		int i = body.indexOf("encoding=\"");
		if(i < 0) return "UTF-8";
		i = i + "encoding=\"".length();
		int j = body.indexOf('"', i);
		if(j < 0) return "UTF-8";
		return body.substring(i, j);
    	 
	}
    public static OutputFormat createOutputFormat(String encoding) {
    	if(encoding == null || encoding.length() == 0) encoding = null; //"UTF-8";
    	//"ISO-8859-1"
        OutputFormat format = new OutputFormat(Method.XML, encoding, true);
        format.setLineSeparator(System.getProperty("line.separator", LineSeparator.Web));
        format.setIndent(1);
        return format;
    }

    public static final boolean serialize(Element element, Writer w) throws IOException {
        if(element == null) return false;
        serialize(element, new XMLSerializer(w, createOutputFormat("UTF-8")));
        w.close();
        return true;
    }

    public static final boolean serialize(Element element, OutputStream w) throws IOException {
        if(element == null) return false;
        serialize(element, new XMLSerializer(w, createOutputFormat("UTF-8")));
        w.close();
        return true;
    }

    public static void serialize(Element element, XMLSerializer serial) throws IOException {
        serial.asDOMSerializer();
        serial.serialize(element);
    }

    public static void serialize(Document document, XMLSerializer serial) throws IOException {
    	if(serial == null || document == null) return;
        serial.asDOMSerializer();
        serial.serialize(document);
    }

    public static final boolean serialize(Document document, Writer w) throws IOException {
    	return serialize(document, w, null);
    }

	public static final boolean serialize(Document document, Writer w, String encoding) throws IOException {
		if(document == null) return false;
		serialize(document, new XMLSerializer(w, createOutputFormat(encoding)));
		w.close();
		return true;
	}

    public static final String getCDATA(Element elem) {
    	return getCDATA(elem, true);
    }

    public static final String getCDATA(Element elem, boolean trim) {
        StringBuffer sb = new StringBuffer();
        NodeList nl = elem.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node nc = nl.item(i);
            if (nc.getNodeType() == Node.CDATA_SECTION_NODE) {
                sb.append(((Text) nc).getData());
            } else if (nc.getNodeType() == Node.TEXT_NODE) {
            	String txt = ((Text) nc).getData();
            	if(trim) txt = txt.trim();
                sb.append(txt);
            }
        }
        return sb.toString();
    }

    public static final String getComment(Element elem) {
        StringBuffer sb = new StringBuffer();
        Node node = elem.getPreviousSibling();
        while (node != null) {
            if (node.getNodeType() == Node.ELEMENT_NODE) break;
            if (node.getNodeType() == Node.COMMENT_NODE) {
                if (sb.length() > 0) {
                    sb.insert(0, '\n');
                    sb.insert(0, ((Comment) node).getData());
                } else {
                    sb.append(((Comment) node).getData());
                }
            }
            node = node.getPreviousSibling();
        }
        return sb.toString();
    }

    public static final void setCDATA(Element element, String data) {
        if (data == null) data = "";
        element.appendChild(element.getOwnerDocument().createCDATASection(data));
    }

    public static final void setText(Element element, String data) {
        if (data == null) data = "";
        element.appendChild(element.getOwnerDocument().createTextNode(data));
    }

    public static final void setComment(Element element, String data) {
        if (data == null) data = "";
        Comment comm = element.getOwnerDocument().createComment(data);
        element.getParentNode().insertBefore(comm, element);
    }

}

