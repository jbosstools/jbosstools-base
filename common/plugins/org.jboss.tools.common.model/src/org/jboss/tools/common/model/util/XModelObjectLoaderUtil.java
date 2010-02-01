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
package org.jboss.tools.common.model.util;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.XChild;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelConstants;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.engines.impl.EnginesLoader;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.impl.XModelImpl;
import org.jboss.tools.common.model.impl.XModelObjectImpl;
import org.jboss.tools.common.model.loaders.XObjectLoader;
import org.jboss.tools.common.model.plugin.ModelMessages;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.common.xml.XMLUtilities;

public class XModelObjectLoaderUtil {
	public static String ATTR_ID_NAME = "_id_"; //$NON-NLS-1$
    private Hashtable<String,String> singular = null;
    private boolean saveentity = true;
    private String namespace = null;
    private NamespaceMapping namespaceMapping = null;
    
    protected String error = null;

    public XModelObjectLoaderUtil() {}

    public void setup(Hashtable<String,String> singular, boolean saveentity) {
        this.singular = singular;
        this.saveentity = saveentity;
        error = null;
    }
    
    public String getError() {
    	return error;
    }
    
    public void setNamespace(String namespace) {
    	this.namespace = namespace;
    }
    
    public void setNamespaceMapping(NamespaceMapping nm) {
    	namespaceMapping = nm;
    }
    
    public void setSaveEntity(boolean b) {
    	saveentity = b;
    }

    public void load(Element element, XModelObject o) {
    	if("AnyElement".equals(o.getModelEntity().getName())) { //$NON-NLS-1$
    		loadAnyElement(element, o);
    	} else {
			loadAttributes(element, o);
			loadChildren(element, o);
			
			if(error == null) {
				Set<String> allowed = getAllowedChildren(o.getModelEntity());
				if(allowed != null && hasUnallowedChildren(element, allowed));
			}
			if(error == null) {
				Set<String> allowed = getAllowedAttributes(o.getModelEntity());
				if(allowed != null && hasUnallowedAttributes(element, allowed));
			}
			
    	}
    }
    
    static Map<XModelEntity, Set<String>> allowedChildren = new HashMap<XModelEntity, Set<String>>();
    
    protected Set<String> getAllowedChildren(XModelEntity entity) {
    	if(entity.getChild("AnyElement") != null) return null; //$NON-NLS-1$
    	Set<String> x = allowedChildren.get(entity);
    	if(x != null) return x;
    	Set<String> children = new HashSet<String>();
    	XAttribute[] as = entity.getAttributes();
    	for (int i = 0; i < as.length; i++) {
    		String xml = as[i].getXMLName();
    		if(xml == null) continue;
    		int s = xml.indexOf('|');
    		if(s > 0) {
    			StringTokenizer st = new StringTokenizer(xml, "|"); //$NON-NLS-1$
    			while(st.hasMoreTokens()) {
    				String dxml = st.nextToken();
        			int k = dxml.indexOf('.');
        			if(k >= 0) children.add(dxml.substring(0, k));
    			}
    		} else {    		
    			int k = xml.indexOf('.');
    			if(k < 0) continue;
    			children.add(xml.substring(0, k));
    		}
    	}
    	XChild[] cs = entity.getChildren();
    	for (int i = 0; i < cs.length; i++) {
    		XModelEntity c = entity.getMetaModel().getEntity(cs[i].getName());
    		if(c == null) continue;
    		String xml = c.getXMLSubPath();
    		if(xml == null || xml.length() == 0) {
    			if(cs[i].isRequired()) {
    				Set<String> a1 = getAllowedChildren(c);
    				if(a1 == null) return null;
    				children.addAll(a1);
    			}
    		}
    		int k = xml.indexOf('.');
    		if(k >= 0) xml = xml.substring(0, k);
    		children.add(xml);
    	}
    	if(children != null) allowedChildren.put(entity, children);
    	return children;
    }

    protected Set<String> getAllowedAttributes(XModelEntity entity) {
    	if(entity.getChild("AnyElement") != null) return null; //$NON-NLS-1$
    	Set<String> attributes = new HashSet<String>();
    	if(saveentity) {
    		attributes.add(XModelConstants.XMODEL_ENTITY_ATTR);
    		attributes.add(XModelConstants.XMODEL_ENTITY_ATTR_OLD);
    	}
    	XAttribute[] as = entity.getAttributes();
    	for (int i = 0; i < as.length; i++) {
    		String xml = as[i].getXMLName();
    		if(xml == null) continue;
    		int s = xml.indexOf('|');
    		if(s > 0) {
    			StringTokenizer st = new StringTokenizer(xml, "|"); //$NON-NLS-1$
    			while(st.hasMoreTokens()) {
    				String dxml = st.nextToken();
        			int k = dxml.indexOf('.');
        			if(k < 0) attributes.add(dxml);
    			}
    		} else {
    			int k = xml.indexOf('.');
    			if(k >= 0) continue;
    			attributes.add(xml);
    		}
    	}
    	return attributes;
    }

    private boolean hasUnallowedChildren(Element element, Set<String> allowed) {
    	NodeList nl = element.getChildNodes();
    	for (int i = 0; i < nl.getLength(); i++) {
    		Node n = nl.item(i);
    		if(n.getNodeType() == Node.ELEMENT_NODE) {
    			String name = n.getNodeName();
       	    	if(namespaceMapping != null) { 
       	    		name = namespaceMapping.convertToDefault(name);
       	    	}
    			if(allowed.contains(name)) continue;
    			error = MessageFormat
						.format(
								"Editor model does not support child element {0} of {1}:0:0",
								name, element.getNodeName());
    			return true;
    		}
    	}
    	return false;
    }

    private boolean hasUnallowedAttributes(Element element, Set<String> allowed) {
    	NamedNodeMap nl = element.getAttributes();
    	for (int i = 0; i < nl.getLength(); i++) {
    		Node n = nl.item(i);
    		if(n.getNodeType() == Node.ATTRIBUTE_NODE) {
    			String name = n.getNodeName();
    			if(allowed.contains(name)) continue;
    			if(name.startsWith("xmlns")) continue; //$NON-NLS-1$
    			error = MessageFormat
						.format(
								"Editor model does not support attribute {0} of {1}:0:0",
								name, element.getNodeName());;
    			return true;
    		}
    	}
    	return false;
    }

    public void loadAttributes(Element element, XModelObject o) {
        XModelEntity entity = o.getModelEntity();
        XAttribute[] as = entity.getAttributes();
        for (int i = 0; i < as.length; i++) {
            String xmlname = as[i].getXMLName();
            if (xmlname == null || xmlname.length() == 0) continue;
            String n = as[i].getName();
            String value = getAttribute(element, xmlname, as[i]);
            if (value != null) o.setAttributeValue(n, value);
            String commentName = getAttributeCommentName(xmlname);
            if(commentName != null) {
            	String fn = xmlname.substring(0, xmlname.indexOf('.'));
            	Element child = XMLUtilities.getFirstChild(element, fn);
            	if(child != null) {
            		String comment = getComment(child);
            		if(comment != null && comment.length() > 0) {
            			o.set(commentName, comment);
            		}
            	}
            }
        }
        String s = getFinalComment(element);
        if(s != null && s.length() > 0) o.set("#final-comment", s); //$NON-NLS-1$
    }
    
    private String applyNamespaceToAttribute(String xmlname) {
    	if(namespaceMapping != null) { 
    		xmlname = namespaceMapping.convertToActual(xmlname);
    	}
    	if(namespace == null || namespace.length() == 0) return xmlname;
    	if(xmlname.indexOf(':') > 0 || xmlname.indexOf('.') >= 0 || xmlname.indexOf('#') >= 0) return xmlname;
    	if(xmlname.equals("xmlns")) { //$NON-NLS-1$
    		return xmlname + ":" + namespace; //$NON-NLS-1$
    	}
    	return xmlname;
    }
    
    protected final String applyNamespaceToTag(String xmlname) {
    	if(namespaceMapping != null) { 
    		xmlname = namespaceMapping.convertToActual(xmlname);
    	}
    	if(namespace != null && namespace.length() > 0) {
    		return namespace + ":" + xmlname; //$NON-NLS-1$
    	}
    	return xmlname;
    }

    //TODO final. getAttribute(Element, String, XAttribute) should be overridden
    public String getAttribute(Element element, String xmlname) {
    	return getAttribute(element, xmlname, null);
    }

    public String getAttribute(Element element, String xmlname, XAttribute attr) {
        if (xmlname.equals("CDATA") || xmlname.equals("#text")) { //$NON-NLS-1$ //$NON-NLS-2$
            return getCDATA(element, attr == null || attr.isTrimmable());
        } else if (xmlname.equals("#comment")) { //$NON-NLS-1$
            return getComment(element);
        } else {
       		int ind = xmlname.indexOf('.');
       		if (ind > 0) {
       			String childName = xmlname.substring(0, ind);
       	    	if(namespaceMapping != null) { 
       	    		childName = namespaceMapping.convertToActual(childName);
       	    	}
       			if(namespace != null && namespace.length() > 0 && childName.indexOf(':') < 0) {
       				childName = namespace + ":" + childName; //$NON-NLS-1$
       			}
       			Element child = XMLUtil.getFirstChild(element, childName);
       			if (child != null) return getAttribute(child, xmlname.substring(ind+1), attr);
       		} else {
               	xmlname = applyNamespaceToAttribute(xmlname);
               	if (XMLUtil.hasAttribute(element, xmlname)) {
               		return element.getAttribute(xmlname);
               	}
        	}
        }
        return null;
    }

    public void loadChildren(Element element, XModelObject o) {
        XModelEntity entity = o.getModelEntity();
        XModel model = o.getModel();
        HashSet<String> childset = entity.getRequiredChildren();
        NodeList nl = element.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node n = nl.item(i);
            if(n.getNodeType() != Node.ELEMENT_NODE) continue;
            Element e = (Element)n;
            String en = getChildEntity(entity, e);
            if(en == null && entity.getChild("AnyElement") != null) en = "AnyElement"; //$NON-NLS-1$ //$NON-NLS-2$
            if(en == null) continue;
            XModelObject co = model.createModelObject(en, null);
            if(co == null) continue;
            load(e, co);
            if(!o.addChild(co) && co.getFileType() == XModelObject.NONE) {
        		if(co.getModelEntity().getAttribute(ATTR_ID_NAME) != null) {
        			int k = 1;
        			String pp = co.getPathPart();
        			while(o.getChildByPath(pp) != null) {
        				co.setAttributeValue(ATTR_ID_NAME, "" + k); //$NON-NLS-1$
        				String ppn = co.getPathPart();
        				if(ppn.equals(pp)) break;
        				pp = ppn;
        				++k;
        			}
        			o.addChild(co);
        		} else if(o.isActive()
        				|| (entity.getChild(en) != null
        						&& entity.getChild(en).isRequired())) try {
            		XModelObject q = o.getChildByPath(co.getPathPart());
            		if(q != null) EnginesLoader.merge(q, co, false);
            	} catch (XModelException exc) {
            		ModelPlugin.getPluginLog().logError("XModelObjectLoaderUtil:loadChildren:" + exc.getMessage(), exc); //$NON-NLS-1$
            	}
            	continue;
            } 
            if(childset != null) childset.remove(en);
        }
        if(childset != null && childset.size() > 0) {
            String[] ens = childset.toArray(new String[childset.size()]);
            for (int i = 0; i < ens.length; i++)
              o.addChild(createValidObject(model, ens[i]));
        }
    }
    
    public static String getModelEntityAttribute(Element element) {
        if(XMLUtil.hasAttribute(element, XModelConstants.XMODEL_ENTITY_ATTR)) {
        	return element.getAttribute(XModelConstants.XMODEL_ENTITY_ATTR);
        }
        if(XMLUtil.hasAttribute(element, XModelConstants.XMODEL_ENTITY_ATTR_OLD)) {
        	return element.getAttribute(XModelConstants.XMODEL_ENTITY_ATTR_OLD);
        }
        return null;
    }

    protected String getChildEntity(XModelEntity entity, Element e) {
		String en = getModelEntityAttribute(e);
		if(en == null || en.length() == 0) {
			String n = e.getNodeName();
   	    	if(namespaceMapping != null) { 
   	    		n = namespaceMapping.convertToDefault(n);
   	    	}
			if(namespace != null && namespace.length() > 0 && n.startsWith(namespace + ":")) { //$NON-NLS-1$
				n = n.substring(namespace.length() + 1);
			}
			en = entity.getChildByXML(n);
		}
		return en;
    }

    protected boolean acceptElement(Element e, String entity) {
        return !saveentity || 
        	(entity.equals(e.getAttribute(XModelConstants.XMODEL_ENTITY_ATTR)) 
        	|| entity.equals(e.getAttribute(XModelConstants.XMODEL_ENTITY_ATTR_OLD)));
    }

    public static XModelObject createValidObject(XModel model, String entityname) {
        return createValidObject(model, entityname, null);
    }    

    public static XModelObject createValidObject(XModel model, String entityname, Properties p) {
        XModelObject o = model.createModelObject(entityname, p);
        if(o != null) addRequiredChildren(o);
        return o;
    }

	public static void addRequiredChildren(XModelObject object) {
		XChild[] cs = object.getModelEntity().getChildren();
		for (int i = 0; i < cs.length; i++) {
			if(cs[i].getMaxCount() != 1 || !cs[i].isRequired()) continue;
			XModelObject c = createValidObject(object.getModel(), cs[i].getName(), null);
			if(c != null) object.addChild(c);
		}
	}

	public static void addRequiredChildren(XModelObject object, boolean onlyHavingNoXMLPath) {
		XChild[] cs = object.getModelEntity().getChildren();
		for (int i = 0; i < cs.length; i++) {
			if(cs[i].getMaxCount() != 1 || !cs[i].isRequired()) continue;
			if(onlyHavingNoXMLPath) {
				XModelEntity e = cs[i].getMetaModel().getEntity(cs[i].getName());
				if(e == null) continue;
				String xml = e.getXMLSubPath();
				if(xml != null && xml.length() > 0) continue;
			}
			XModelObject c = createValidObject(object.getModel(), cs[i].getName(), null);
			if(c != null) object.addChild(c);
		}
	}

    public boolean save(Element parent, XModelObject o) {
		if("AnyElement".equals(o.getModelEntity().getName())) { //$NON-NLS-1$
			saveAnyElement(parent, o);
			return true;
		} else {
			String xmlname = getXMLSubPath(o);
			if(xmlname == null || xmlname.trim().length() == 0) return true;
   	    	if(namespaceMapping != null) { 
   	    		xmlname = namespaceMapping.convertToActual(xmlname);
   	    	}
			if(namespace != null && namespace.length() > 0) {
				xmlname = namespace + ":" + xmlname; //$NON-NLS-1$
			}
			Element element = XMLUtil.createElement(parent, xmlname);
			saveAttributes(element, o);
			boolean b = saveChildren(element, o);
			saveFinalComment(element, o);
			if(b) o.setModified(false);
			return b;
		}
    }
    
    protected String getXMLSubPath(XModelObject o) {
    	return o.getModelEntity().getXMLSubPath();
    }

    public void saveAttributes(Element element, XModelObject o) {
        XModelEntity entity = o.getModelEntity();
        XAttribute[] as = entity.getAttributes();
        if (saveentity) {
            element.setAttribute(XModelConstants.XMODEL_ENTITY_ATTR, entity.getName());
        }
        for (int i = 0; i < as.length; i++) {
            if (as[i].isFake()) continue;
            String xmlname = as[i].getXMLName();
            if (xmlname == null || xmlname.length() == 0) continue;
            String n = as[i].getName();
            String v = o.getAttributeValue(n);
            if (isSaveable(entity, n, v, as[i].getDefaultValue())) {
                saveAttribute(element, xmlname, v);
                String commentName = getAttributeCommentName(xmlname);
                if(commentName != null) {
                	String comment = o.get(commentName);
                	if(comment != null && comment.length() > 0) {
                		String cn = xmlname.substring(0, xmlname.indexOf('.'));
                		Element child = XMLUtilities.getLastChild(element, cn);
                		if(child != null) {
                			setComment(child, comment);
                		}
                	}                		
                }
            }
        }
    }

    public void saveAttribute(Element element, String xmlname, String value) {
        if (xmlname.equals("CDATA")) { //$NON-NLS-1$
            setCDATA(element, value);
        } else if (xmlname.equals("#text")) { //$NON-NLS-1$
            setText(element, value);
        } else if (xmlname.equals("#comment")) { //$NON-NLS-1$
            setComment(element, value);
        } else {
            int ind = xmlname.indexOf('.');
            if (ind > 0) {
                String childName = xmlname.substring(0, ind);
       	    	if(namespaceMapping != null) { 
       	    		childName = namespaceMapping.convertToActual(childName);
       	    	}
                if(namespace != null && namespace.length() > 0 && childName.indexOf(':') < 0) {
                	childName = namespace + ":" + childName; //$NON-NLS-1$
                }
                Element child = XMLUtil.getFirstChild(element, childName);
                if (child == null) child = XMLUtil.createElement(element, childName);
                saveAttribute(child, xmlname.substring(ind+1), value);
            } else {
            	xmlname = applyNamespaceToAttribute(xmlname);
                element.setAttribute(xmlname, value);
            }
        }
    }
    
    protected boolean isSaveable(XModelEntity entity, String n, String v, String dv) {
        if (v == null || v.length() == 0) return false;
        if (!v.equals(dv)) return true;
        return (singular != null && singular.get(n) != null);
    }
    
    public void saveFinalComment(Element element, XModelObject o) {
    	String data = o.get("#final-comment"); //$NON-NLS-1$
    	if(data == null || data.length() == 0) return;
        Comment comm = element.getOwnerDocument().createComment(data);
        element.appendChild(comm);    	
    }

    public boolean saveChildren(Element element, XModelObject o) {
        XModelObject[] os = ((XModelObjectImpl)o).getChildrenForSave();
        boolean b = true;
        for (int i = 0; i < os.length; i++) if(!save(element, os[i])) b = false;
        return b;
    }

    public XModelObject parse(XModel model, Reader reader) {
        Element element = XMLUtil.getElement(reader);
        if(element == null) return null;
        String entity = getModelEntityAttribute(element);
        XModelObject o = model.createModelObject(entity, null);
        load(element, o);
        return o;
    }

    public Element asElement(XModelObject o) {
        Element root = XMLUtil.createDocumentElement(o.getModelEntity().getXMLSubPath());
        saveAttributes(root, o);
        boolean b = saveChildren(root, o);
////        if(o instanceof XSavable && !((XSavable)o).save()) b = false;
        return (b) ? root : null;
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
		if(body == null || body.length() == 0) return null; //"UTF-8"
		String encoding = FileUtil.getEncoding(body);
		if(encoding == null) return null; //"UTF-8"
		return FileUtil.validateEncoding(encoding, "UTF-8"); //$NON-NLS-1$
	}

    public static OutputFormat createOutputFormat(String encoding) {
    	return XMLUtilities.createOutputFormat(encoding);
    }

    public static final boolean serialize(Element element, Writer w) throws IOException {
        if(element == null) return false;
        serialize(element, new XMLSerializer(w, createOutputFormat("UTF-8"))); //$NON-NLS-1$
        w.close();
        return true;
    }

    public boolean serialize(XModelObject object, Writer w) throws XModelException, IOException {
        return serialize(asElement(object), w);
    }

    public static final boolean serialize(Element element, OutputStream w) throws IOException {
        if(element == null) return false;
        serialize(element, new XMLSerializer(w, createOutputFormat("UTF-8"))); //$NON-NLS-1$
        w.close();
        return true;
    }

    public boolean serialize(XModelObject object, OutputStream w) throws XModelException, IOException {
        return serialize(asElement(object), w);
    }

    public static void serialize(Element element, XMLSerializer serial) throws IOException {
        serial.asDOMSerializer();
        serial.serialize(element);
    }

    public static void serialize(Document document, XMLSerializer serial) throws IOException {
        serial.asDOMSerializer();
        serial.serialize(document);
    }

    public static final boolean serialize(Document document, Writer w) throws IOException {
    	return serialize(document, w, null);
    }

	public static final boolean serialize(Document document, Writer w, String encoding) throws IOException, IOException {
		if(document == null) return false;
		serialize(document, new XMLSerializer(w, createOutputFormat(encoding)));
		w.close();
		return true;
	}

    public String asString(XModelObject object) {
        StringWriter w = new StringWriter();
        Exception e = null;
        try {
            serialize(object, w);
            return w.toString();
        } catch (XModelException e1) {
        	e = e1;
        } catch (IOException e2) {
        	e = e2;
        }
    	ModelPlugin.getPluginLog().logError("XModelObjectLoaderUtil:asString:" + e.getMessage(), e); //$NON-NLS-1$
    	return ""; //$NON-NLS-1$
    }

    public static final String getCDATA(Element elem) {
    	return XMLUtilities.getCDATA(elem, true);
    }

    public static final String getCDATA(Element elem, boolean trim) {
    	return XMLUtilities.getCDATA(elem, trim);
    }

    public static final String getComment(Element elem) {
        StringBuffer sb = new StringBuffer();
        Node node = elem.getPreviousSibling();
        loadComment(sb, node);
        return sb.toString();
    }

    public static final String getFinalComment(Element elem) {
        StringBuffer sb = new StringBuffer();
        NodeList nl = elem.getChildNodes();
        if(nl == null || nl.getLength() == 0) return ""; //$NON-NLS-1$
        Node node = nl.item(nl.getLength() - 1);
        loadComment(sb, node);
        return sb.toString();
    }

    static final void loadComment(StringBuffer sb, Node node) {
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
    }

    public static final void setCDATA(Element element, String data) {
        if (data == null) data = ""; //$NON-NLS-1$
        element.appendChild(element.getOwnerDocument().createCDATASection(data));
    }

    public static final void setText(Element element, String data) {
        if (data == null) data = ""; //$NON-NLS-1$
        element.appendChild(element.getOwnerDocument().createTextNode(data));
    }

    public static final void setComment(Element element, String data) {
        if (data == null) data = ""; //$NON-NLS-1$
        Comment comm = element.getOwnerDocument().createComment(data);
        element.getParentNode().insertBefore(comm, element);
    }

    public void load(File f, XModelObject o) {
    	if(f == null || !f.isFile()) return;
        Element element = XMLUtil.getElement(f.getAbsolutePath());
        if(element != null) {
        	load(element, o);
        }
    }

    public boolean save(File f, XModelObject o) {
        if(f.exists() && !o.isModified()) return true;
        StringWriter w = new StringWriter();
        Exception e = null;
        try {
            if(!serialize(o, w)) return false;
        } catch (XModelException e1) {
        	e = e1;
        } catch (IOException e2) {
        	e = e2;
        }
        if(e != null) {
        	ModelPlugin.getPluginLog().logError("XModelObjectLoaderUtil:save(f,o):" + e.getMessage(), e); //$NON-NLS-1$
            return false;
        }
        String r = w.toString();
        boolean c = coincides(f, r);
        int i = (c) ? 0 : handleReadOnly(f, o);
        if(i != 0) return (i == 1);
        if(!c) writeFile(f, r);
        o.setModified(false);
        return true;
    }

    public static boolean saveBody(File f, XModelObject o, String defaultEncoding) {
        boolean same = FileUtil.isSameFile(f);
        if(same && !o.isModified()) return true;
        String r = getTempBody(o);
        boolean c = same && coincides(f, r);
        int i = (c) ? 0 : handleReadOnly(f, o);
        if(i != 0) return (i == 1);
        if(!c) {
       		if(!FileUtil.writeFileWithEncodingCheck(f, r, defaultEncoding)) return false;
        }
        o.setModified(false);
        return true;
    }

    public static int handleReadOnly(File f, XModelObject o) {
        int i = 0;
        while(i == 0 && f.exists() && !f.canWrite())
          i = o.getModel().getService().showDialog("Question",
                getReadOnlyMessage(f), new String[]{"Retry", "Discard", ModelMessages.Cancel}, null,
                ServiceDialog.QUESTION);
        return i;
    }

    public static String getReadOnlyMessage(File f) {
        return MessageFormat.format("Cannot save to read-only file {0}.", f.getAbsolutePath());
    }

    public static String getTempBody(XModelObject o) {
        String b = o.get(XModelObjectConstants.ATTR_NAME__BODY_);
        o.set(XModelObjectConstants.ATTR_NAME__BODY_, ""); //$NON-NLS-1$
        return b;
    }

    public static void setTempBody(XModelObject o, String body) {
        o.set(XModelObjectConstants.ATTR_NAME__BODY_, body);
    }

    //////pass correct properties!

    private static String expandString(String s) {
        return Paths.expand(s, System.getProperties());
    }

    public static String readFile(String filename) {
    	if(filename == null) return ""; //$NON-NLS-1$
        return readFile(new File(expandString(filename)));
    }

    public static String readFile(File f) {
        return FileUtil.readFile(f);
    }

    public static boolean isTextFile(File f, int length) {
        return FileUtil.isTextFile(f, length);
    }

    public static boolean writeFile(String filename, String value) {
    	if(filename == null) return false;
        return writeFile(new File(expandString(filename)), value);
    }

    public static boolean writeFile(File f, String value) {
        return FileUtil.writeFile(f, value);
    }

    public static boolean coincides(File f, String value) {
        return f.isFile() && value != null && readFile(f).equals(value);
    }

    public static XObjectLoader getObjectLoader(XModelObject object) {
    	if(object == null) return null;
    	return object.getModelEntity().getObjectLoader();
    }

    public static void remove(File f) {
        FileUtil.remove(f);
    }

    public static final String getResourcePath(XModelObject object) {
        int t = object.getFileType();
        if(t == XFileObject.SYSTEM) return ""; //$NON-NLS-1$
        XModelObject p = object.getParent();
        if(p == null) return null;
        String n = (t == XFileObject.FOLDER) ? object.get(XModelObjectConstants.XML_ATTR_NAME) :
                   (t == XFileObject.FILE) ? FileAnyImpl.toFileName(object) :
                   object.getPathPart();
        String pp = getResourcePath(p);
        return (pp == null) ? n : pp + XModelObjectConstants.SEPARATOR + n;
    }

    /*
     * Multilined text may be saved as xml attribute value
     * by coding chars '\\', '\n' and '\t'
     * with strings "\\\\", "\\n" and "\\t"
     * Editor for this attribute uses load-method to present
     * true value of attribute and save-method to write
     * modified value. 
     */

    public static String loadFromXMLAttribute(String text) {
        StringBuffer sb = new StringBuffer();
        char[] c = text.toCharArray();
        boolean slash = false;
        for (int i = 0; i < c.length; i++) {
            if(!slash) {
                if(c[i] != '\\') {
                    sb.append(c[i]);
                } else if(c[i] == '\\') {
                    slash = true;
                }
            } else {
                slash = false;
                if(c[i] == '\\') sb.append('\\');
                else if(c[i] == 'n') sb.append('\n');
                else if(c[i] == 't') sb.append('\t');
                else sb.append('\\').append(c[i]);
            }
        }
        return sb.toString();
    }

    public static String saveToXMLAttribute(String text) {
        StringBuffer sb = new StringBuffer();
        char[] c = text.toCharArray();
        for (int i = 0; i < c.length; i++) {
            if(c[i] == '\r') continue;
            else if(c[i] == '\n') sb.append('\\').append('n');
            else if(c[i] == '\\') sb.append('\\').append('\\');
            else if(c[i] == '\t') sb.append('\\').append('t');
            else sb.append(c[i]);
        }
        return sb.toString();
    }
    
    //

	public static void updateModifiedOnSave(XModelObject o) {
		if(!o.isModified()) updateModifiedFlag(o.getParent());
	}
    
	public static void updateModifiedFlag(XModelObject o) {
		if(o == null) return;
		XModelObject[] cs = o.getChildren();
		for (int i = 0; i < cs.length; i++) if(cs[i].isModified()) return;
		o.setModified(false);
		updateModifiedFlag(o.getParent());
	}
	
	protected void loadAnyElement(Element element, XModelObject o) {
		o.setAttributeValue("tag", element.getTagName()); //$NON-NLS-1$
		StringBuffer sb = new StringBuffer();
		NamedNodeMap as = element.getAttributes();
		for (int i = 0; i < as.getLength(); i++) {
			Node n = as.item(i);
			String nm = n.getNodeName();
			String v = n.getNodeValue();
			if(v == null) continue;
			if(sb.length() > 0) sb.append(";"); //$NON-NLS-1$
			sb.append(nm).append("=").append(v); //$NON-NLS-1$
		}
		o.setAttributeValue("attributes", sb.toString()); //$NON-NLS-1$
		String text = getAttribute(element, "#text").trim(); //$NON-NLS-1$
		if(text.length() > 0) {
			while(text.startsWith("\n") || text.startsWith("\r")) text = text.substring(1); //$NON-NLS-1$ //$NON-NLS-2$
			while(text.endsWith("\n") || text.endsWith("\r")) text = text.substring(0, text.length() - 1); //$NON-NLS-1$ //$NON-NLS-2$
			o.setAttributeValue("text", text); //$NON-NLS-1$
		}
		loadChildren(element, o);
	}
    
	protected void saveAnyElement(Element parent, XModelObject o) {
		String xmlname = o.getAttributeValue("tag"); //$NON-NLS-1$
		if(xmlname == null || xmlname.trim().length() == 0) return;
		if(namespace != null && namespace.length() > 0) {
			xmlname = namespace + ":" + xmlname; //$NON-NLS-1$
		}
		Element element = XMLUtil.createElement(parent, xmlname);
		String attrs = o.getAttributeValue("attributes"); //$NON-NLS-1$
		StringTokenizer st = new StringTokenizer(attrs, ";"); //$NON-NLS-1$
		while(st.hasMoreTokens()) {
			String t = st.nextToken();
			int i = t.indexOf('=');
			if(i < 0) continue;
			String n = t.substring(0, i);
			String v = t.substring(i + 1);
			element.setAttribute(n, v);
		}
		String text = o.getAttributeValue("text"); //$NON-NLS-1$
		if(text != null && text.length() > 0) {
			saveAttribute(element, "#text", text); //$NON-NLS-1$
		}
		XModelObject[] cs = o.getChildren();
		for (int i = 0; i < cs.length; i++) {
			saveAnyElement(element, cs[i]);
		}
	}

	protected void eitherOr(Element element, String attr1, String attr2) {
		Element e1 = XMLUtil.getUniqueChild(element, attr1);
		Element e2 = XMLUtil.getUniqueChild(element, attr2);
		if(e1 != null && e2 != null) element.removeChild(e2);
	}

	protected void saveArray(Element element, String xmlname, String value) {
		String[] ns = XModelObjectUtil.asStringArray(value);
    	if(namespaceMapping != null) { 
    		xmlname = namespaceMapping.convertToActual(xmlname);
    	}
		if(namespace != null && namespace.length() > 0 && xmlname.indexOf(':') < 0) {
			xmlname = namespace + ":" + xmlname; //$NON-NLS-1$
		}
		for (int i = 0; i < ns.length; i++) {
			Element c = XMLUtil.createElement(element, xmlname);
			saveAttribute(c, "#text", ns[i]); //$NON-NLS-1$
		}
	}

	protected String loadArray(Element element, String xmlname) {
		Element[] es = XMLUtil.getChildren(element, xmlname);
		if(es == null || es.length == 0) return ""; //$NON-NLS-1$
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < es.length; i++) {
			if(i > 0) sb.append(","); //$NON-NLS-1$
			sb.append(getAttribute(es[i], "#text")); //$NON-NLS-1$
		}
		return sb.toString();
	}

	public static void mergeAttributes(XModelObject destination, XModelObject source) throws XModelException {
		mergeAttributes(destination, source, destination.isActive());
	}
	
	public static void mergeAttributes(XModelObject destination, XModelObject source, boolean fire) throws XModelException {
		XAttribute[] as = destination.getModelEntity().getAttributes();
		for (int i = 0; i < as.length; i++) {
			String n = as[i].getName();
			if(!as[i].isCopyable()) continue;
			mergeAttributeComment(destination, source, as[i], fire);
			String ov = destination.getAttributeValue(n);
			String nv = source.getAttributeValue(n);
			if(nv == null || (ov != null && ov.equals(nv))) continue;
			if(fire) {
				destination.getModel().changeObjectAttribute(destination, n, nv);
			} else {
				destination.setAttributeValue(n, nv);
			}
		}
		mergeFinalComment(destination, source, fire);
	}
	
	public static void mergeAttributeComment(XModelObject destination, XModelObject source, XAttribute attr, boolean fire) {
		String commentName = getAttributeCommentName(attr.getXMLName());
		mergeComment(destination, source, commentName, fire);
	}
	
	private static String getAttributeCommentName(String xmlname) {
        if(xmlname != null && xmlname.indexOf('.') > 0 && !xmlname.endsWith("#comment")) { //$NON-NLS-1$
			return xmlname + ".#comment"; //$NON-NLS-1$
		}
        return null;		
	}

	public static void mergeFinalComment(XModelObject destination, XModelObject source, boolean fire) {
		String commentName = "#final-comment"; //$NON-NLS-1$
		mergeComment(destination, source, commentName, fire);
	}
	
	static void mergeComment(XModelObject destination, XModelObject source, String commentName, boolean fire) {
		if(commentName == null) return;
		String newComment = source.get(commentName);
		String oldComment = destination.get(commentName);
		String set = null;
		if(newComment == null || newComment.length() == 0) {
			if(oldComment != null && oldComment.length() > 0) {
				set = ""; //$NON-NLS-1$
			} 
		} else {
			if(oldComment == null || !oldComment.equals(newComment)) {
				set = newComment;
			}
		}
		if(set != null) {
			destination.set(commentName, set);
			if(fire) {
				XModelImpl impl = (XModelImpl)destination.getModel();
				impl.fireNodeChanged(destination, destination.getPath());
			}
		}
	}
	
}
