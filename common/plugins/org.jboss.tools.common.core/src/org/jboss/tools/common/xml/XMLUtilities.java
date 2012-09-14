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

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xml.serialize.LineSeparator;
import org.apache.xml.serialize.Method;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.jboss.tools.common.core.CommonCorePlugin;
import org.jboss.tools.common.core.Messages;
import org.w3c.dom.Comment;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

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
        return al.toArray(new Element[al.size()]);
    }

    public static Element getUniqueChild(Element parent, String name){
        return getFirstChild(parent, name);
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
        if(i < 0) {
			return getChildren(parent, name);
		}
        Element p = getUniqueChild(parent, name.substring(0, i));
        return (p == null) ? new Element[0] : getAncestors(p, name.substring(i + 1));
    }

    public static Element createElement(Element parent, String path) {
        int i = path.indexOf('.');
        Element element = null;
        if(i < 0) {
            element = parent.getOwnerDocument().createElement(path);
            parent.appendChild(element);
        } else {
	        String p = path.substring(0, i), c = path.substring(i + 1);
	        Element pe = getUniqueChild(parent, p);
	        if(pe == null) {
	            pe = parent.getOwnerDocument().createElement(p);
	            parent.appendChild(pe);
	        }
	        element = createElement(pe, c);
        }
        return element;
    }

	public static DocumentBuilder createDocumentBuilder() {
		return  createDocumentBuilder(false);
	}

	public static DocumentBuilder createDocumentBuilder(boolean validate) {
		try {
			DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
			f.setValidating(validate);
			// / f.setExpandEntityReferences(false);
			DocumentBuilder d = f.newDocumentBuilder();
			if (!validate) {
				d.setEntityResolver(EMPTY_RESOLVER);
			}
			d.setErrorHandler(new ErrorHandlerImpl());
			return d;
		} catch (ParserConfigurationException e) {
			CommonCorePlugin.getPluginLog().logError(e);
		}
		return null;
	}
	
	public static final EntityResolver EMPTY_RESOLVER = createEmptyEntityResolver();
	
	

    public static Element createDocumentElement(String name) {
		Document d = createDocumentBuilder().newDocument();
        Element de = d.createElement(name);
        d.appendChild(de);
        return de;
    }

    public static EntityResolver createEmptyEntityResolver() {
		return new EntityResolver() {
			public InputSource resolveEntity(java.lang.String publicId, java.lang.String systemId) throws SAXException, java.io.IOException {
				if((systemId != null) && 
						(systemId.toLowerCase().endsWith(".dtd") || systemId.toLowerCase().endsWith(".ent"))) { // this deactivates DTD //$NON-NLS-1$ //$NON-NLS-2$
					return new InputSource(new ByteArrayInputStream("<?xml version='1.0' encoding='UTF-8'?>".getBytes())); //$NON-NLS-1$
				} else {
					return null;
				}
			}
		};
	}

	public static Element createDocumentElement(String name, String qName, String publicId, String systemId, String namespaceURI) {
        Document d = null;
        try {
            DOMImplementation domImpl = createDocumentBuilder().getDOMImplementation();
            DocumentType docType = domImpl.createDocumentType(qName, publicId, systemId);
            d = domImpl.createDocument(namespaceURI, name, docType);
        } catch (DOMException e) {
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
            return getElement(new File(filename), resolver);
    }

    public static Element getElement(File file, EntityResolver resolver) {
    	if(file == null || !file.isFile()) {
			return null;
		}
    	java.io.FileReader fr = null;
    	try {
    		fr = new java.io.FileReader(file);
        	org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(fr);
        	return getElement(inSource, resolver);
        } catch (FileNotFoundException e) {
        	CommonCorePlugin.getPluginLog().logError(e);
		} catch (IOException e) {
        	CommonCorePlugin.getPluginLog().logError(e);
		} catch (SAXException e) {
        	CommonCorePlugin.getPluginLog().logError(e);
		} finally {
            try {
                if (fr != null) {
					fr.close();
				}
            } catch (IOException e) {
            	CommonCorePlugin.getPluginLog().logError(e);
            }
        }
		return null;
    }

    public static Element getElement(Reader reader, EntityResolver resolver) {
        Document d = getDocument(reader, resolver);
        return (d == null) ? null : d.getDocumentElement();
    }

    public static Document getDocument(Reader reader, EntityResolver resolver) {
        try {
            org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(reader);
            return getDocument(inSource, resolver);
        } catch (SAXException e) {
            return null;
        } catch (IOException e) {
            return null;
        }
    }

    public static Element getElement(InputStream is, EntityResolver resolver) {
        try {
            org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(is);
            return getElement(inSource, resolver);
        } catch (SAXException e) {
            return null;
        } catch (IOException e) {
            return null;
        }
    }

    public static Element getElement(InputSource is, EntityResolver resolver) throws SAXException, IOException  {
        return getDocument(is, resolver).getDocumentElement();
    }
    
    public static Document getDocument(InputSource is, EntityResolver resolver) throws SAXException, IOException{
		DocumentBuilder builder = createDocumentBuilder(false);
		if(resolver != null) {
			builder.setEntityResolver(resolver);
		}
		return builder.parse(is);
    }

	public static String[] getXMLErrors(Reader reader, EntityResolver resolver) {
		return getXMLErrors(reader, true, resolver);
	}
    
    public static String[] getXMLErrors(Reader reader, boolean checkDTD, EntityResolver resolver) {
        org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(reader);
        return getXMLErrors(inSource, checkDTD, resolver);
    }
    
	public static String[] getXMLErrors(org.xml.sax.InputSource is, EntityResolver resolver) {
		return getXMLErrors(is, true, resolver);
	}
		
    public static String[] getXMLErrors(org.xml.sax.InputSource is, boolean checkDTD, EntityResolver resolver) {
		ErrorHandlerImpl h = new ErrorHandlerImpl();
        try {
			DocumentBuilder builder = createDocumentBuilder(checkDTD);
            if(resolver != null) {
				builder.setEntityResolver(resolver);
			}
            builder.setErrorHandler(h);
            builder.parse(is);
        } catch (IOException e) {
        	if(h.errors.isEmpty()) {
				return new String[]{Messages.XMLUtilities_IOExceptionMessage+":0:0",e.toString()}; //$NON-NLS-1$
			}
        } catch (SAXException e) {
        	if(h.errors.isEmpty()) {
				return new String[]{Messages.XMLUtilities_SAXExceptionMessage+":0:0",e.toString()}; //$NON-NLS-1$
			}
		}
        return h.errors.toArray(new String[h.errors.size()]);        
    }
    
    public static final void serialize(Element element, String filename) throws IOException {
        File f = new File(filename);
        if(f.exists() && !f.canWrite()) {
			return;
		}
        if(!f.exists()) {
			f.createNewFile();
		}
        FileWriter fw = new FileWriter(f);
        serialize(element, new BufferedWriter(fw));
        fw.close();
    }
	static final String ENCODING = "encoding=\""; //$NON-NLS-1$
	static final String UTF8 = "UTF-8"; //$NON-NLS-1$
	
	public static String getEncoding(String body) {
		int i = body.indexOf(ENCODING);
		if(i < 0) {
			return UTF8;
		}
		i = i + ENCODING.length();
		int j = body.indexOf('"', i);
		if(j < 0) {
			return UTF8;
		}
		return body.substring(i, j);
    	 
	}
    public static OutputFormat createOutputFormat(String encoding) {
        OutputFormat format = new OutputFormat(Method.XML, encoding == null || encoding.length() == 0?null:encoding, true);
        format.setLineSeparator(System.getProperty("line.separator", LineSeparator.Web)); //$NON-NLS-1$
        format.setIndent(1);
        return format;
    }

    public static final boolean serialize(Element element, Writer w) throws IOException {
        if(element == null) {
			return false;
		}
        serialize(element, new XMLSerializer(w, createOutputFormat(UTF8)));
        w.close();
        return true;
    }

    public static final boolean serialize(Element element, OutputStream w) throws IOException {
        if(element == null) {
			return false;
		}
        serialize(element, new XMLSerializer(w, createOutputFormat(UTF8)));
        w.close();
        return true;
    }

    public static void serialize(Element element, XMLSerializer serial) throws IOException {
        serial.asDOMSerializer();
        serial.serialize(element);
    }

    public static void serialize(Document document, XMLSerializer serial) throws IOException {
    	if(serial == null || document == null) {
			return;
		}
        serial.asDOMSerializer();
        serial.serialize(document);
    }

    public static final boolean serialize(Document document, Writer w) throws IOException {
    	return serialize(document, w, null);
    }

	public static final boolean serialize(Document document, Writer w, String encoding) throws IOException {
		if(document == null) {
			return false;
		}
		serialize(document, new XMLSerializer(w, createOutputFormat(encoding)));
		w.close();
		return true;
	}

    public static final String getCDATA(Element elem) {
    	return getCDATA(elem, true);
    }

    public static final String getCDATA(Element elem, boolean trim) {
    	StringBuilder sb = new StringBuilder();
        NodeList nl = elem.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node nc = nl.item(i);
            if (nc.getNodeType() == Node.CDATA_SECTION_NODE) {
                sb.append(((Text) nc).getData());
            } else if (nc.getNodeType() == Node.TEXT_NODE) {
            	String txt = ((Text) nc).getData();
            	if(trim) {
					txt = txt.trim();
				}
                sb.append(txt);
            }
        }
        return sb.toString();
    }

    public static final String getComment(Element elem) {
    	StringBuilder sb = new StringBuilder();
        Node node = elem.getPreviousSibling();
        while (node != null) {
            if (node.getNodeType() == Node.ELEMENT_NODE) {
				break;
			}
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
        element.appendChild(element.getOwnerDocument().createCDATASection(
        												data!=null?data:"")); //$NON-NLS-1$
    }

    public static final void setText(Element element, String data) {
        element.appendChild(element.getOwnerDocument().createTextNode(
														data!=null?data:"")); //$NON-NLS-1$
    }

    public static final void setComment(Element element, String data) {
        Comment comm = element.getOwnerDocument().createComment(
				data!=null?data:""); //$NON-NLS-1$
        element.getParentNode().insertBefore(comm, element);
    }

}

