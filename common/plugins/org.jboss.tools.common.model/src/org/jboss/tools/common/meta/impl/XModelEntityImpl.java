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
package org.jboss.tools.common.meta.impl;

import java.util.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.loaders.XObjectLoader;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.xml.XMLUtilities;

public class XModelEntityImpl extends XMetaElementImpl implements XModelEntity {
	public static boolean hideHelp = true;
    protected XChildrenImpl children = new XChildrenImpl();
    protected XAttribute[] m_Attributes;
    protected XActionListImpl actions = new XActionListImpl();
    protected XEntityRenderer m_Renderer = null;
    protected ClassHolder implementation = null;
    protected String implementationClassName = null;
    protected String m_GeneratorClassName = null;
    protected ClassHolder loader = null;
    protected String loaderClassName = null;
    protected String m_EditorClassName = null;
    protected XDependencies dependencies = new DefaultDependencies();
    protected XAdoptManager adopt = null;

    protected String xmlSubPath = null;
    protected Element element = null;

    protected XModelEntityImpl() {}

    void setElement(Element element) {
        this.element = element;
        setName(element.getAttribute(NAME));
    }

    void validate() {
        if(element == null) return;
        synchronized(this) {
            if(element == null) return;
            load(element);
            element = null;
            XModelMetaDataImpl meta = (XModelMetaDataImpl)getMetaModel();
            ArrayList es = meta.getExtensions().getExtensions(getName());
            if(es != null) {
                for (int i = 0; i < es.size(); i++)
                  merge((XModelEntityExtensionImpl)es.get(i));
                if(!hideHelp) addHelpAction();
            }
        }
    }
    
    private void addHelpAction() {
    	XActionListImpl list = actions;
		XActionItem item = actions.getItem("Properties");
		if(item instanceof XActionListImpl) list = (XActionListImpl)item;
		list.addActionItem(XActionImpl.getHelpAction());    	
    }

    public XAttribute[] getAttributes(){
       return m_Attributes;
    }

    public XAttribute getAttribute(String name) {
        for (int i = 0; i < m_Attributes.length; i++)
          if(m_Attributes[i].getName().equals(name)) return m_Attributes[i];
        return null;
    }

    public void setAttributes(XAttribute[] attrs){
       m_Attributes=attrs;
       if(attrs != null) for (int i = 0; i < attrs.length; i++) {
          XAttributeImpl ai = (XAttributeImpl)attrs[i];
          ai.setModelEntity(this);
       }
    }
    
    public Class getImplementingClass(){
        return implementation.getHoldedClass();
    }

    public void setImplementingClassName(String className){
       implementation = new ClassHolder(className, this, "Implementations");
       implementationClassName = className;
    }

    public Class getLoadingClass() {
        return (loader == null) ? null : loader.getHoldedClass();
    }

    public void setLoaderClassName(String className) {
       if(className != null && className.length() > 0) {
         loader = new ClassHolder(className, this, "Loaders");
         loaderClassName = className;
       }
    }

    public String getEditorClassName() {
        return (m_EditorClassName = expand(m_EditorClassName, "ObjectEditor"));
    }

    public void setEditorClassName(String className){
       m_EditorClassName = className;
    }

    public String getGeneratorClassName() {
       return m_GeneratorClassName;
    }
    public void setGeneratorClassName(String className) {
       m_GeneratorClassName = className;
    }

    public XActionList getActionList() {
        if(!actions.isLoaded()) {
            actions.validate();
            if(!hideHelp) addHelpAction();
        }
        return actions;
    }

    public XChild getChild(String entityName) {
        return children.getChild(entityName);
    }

    public XChild[] getChildren() {
        return children.getChildren();
    }

    public XEntityRenderer getRenderer() {
        return m_Renderer;
    }

    public void setRenderer(XEntityRenderer renderer) {
        m_Renderer = renderer;
    }

    public String getXMLSubPath() {
        return xmlSubPath;
    }

    public void load(Element element) {
        setName(element.getAttribute(NAME));
        setImplementingClassName(element.getAttribute(IMPLEMENTING_CLASS));
        setLoaderClassName(element.getAttribute(IMPLEMENTATION_LOADING_CLASS));
        if(XMetaDataLoader.hasAttribute(element, OBJECT_EDITOR_CLASSNAME)) {
            setEditorClassName(element.getAttribute(OBJECT_EDITOR_CLASSNAME));
        }
        if(XMetaDataLoader.hasAttribute(element, IMPLEMENTATION_GENERATOR_CLASS)) {
            setGeneratorClassName(element.getAttribute(IMPLEMENTATION_GENERATOR_CLASS));
        }
//        setAttributes((XAttribute[])XMetaDataLoader.loadElementGroup(element,XMODEL_ATTRIBUTES, XMODEL_ATTRIBUTE, XAttributeImpl.class));
        loadAttributes(element);
        children.load(element);
        setRenderer((XEntityRenderer)XMetaDataLoader.loadMetaElement(element,RENDERER, XEntityRendererImpl.class, true));
        if(XMetaDataLoader.hasAttribute(element, ADOPT_MANAGER_CLASS)) {
            setAdoptManager(element.getAttribute(ADOPT_MANAGER_CLASS));
        }
        loadDependencies(element);
        xmlSubPath = element.getAttribute("XMLSUBPATH");
        Element ei = XMetaDataLoader.getUniqueChild(element, "XActionItem");
        actions.setElement(ei);
		loadProperties(element);
    }
    
    private void loadAttributes(Element element) {
    	Element att = XMetaDataLoader.getUniqueChild(element, XMODEL_ATTRIBUTES);
    	NodeList nl = att.getChildNodes();
    	List<XAttributeImpl> list = new ArrayList<XAttributeImpl>();
    	for (int i = 0; i < nl.getLength(); i++) {
    		Node n = nl.item(i);
    		if(n.getNodeType() == Node.ELEMENT_NODE) {
    			Element e = (Element)n;
    			String name = e.getNodeName();
    			if(XMODEL_ATTRIBUTE.equals(name)) {
    				XAttributeImpl attr = (XAttributeImpl)XMetaDataLoader.loadMetaElement(e, XAttributeImpl.class, false);
    	             if(attr != null) list.add(attr);
    			} else if(XMODEL_ATTRIBUTE_REF.equals(name)) {
    				String entityName = e.getAttribute("entity");
    				String attrName = e.getAttribute(NAME);
    				String attributes = e.getAttribute("attributes");
    				String[] atributeNames = new String[]{attrName};
    				if(atributeNames != null && attributes.length() > 0) {
    					atributeNames = XModelObjectUtil.asStringArray(attributes);
    				}
    				XModelEntity entity = getMetaModel().getEntity(entityName);
    				if(entity != null) {
    					for (int k = 0; k < atributeNames.length; k++) {
        					XAttributeImpl attr = (XAttributeImpl)entity.getAttribute(atributeNames[k]);
        					if(attr != null) {
        						list.add(attr.copy());
        					} else {
        						ModelPlugin.log("Cannot find reference to attribute " + attrName + " of entity " + entityName);
        					}
    					}
    				} else {
						ModelPlugin.log("Cannot find reference to entity " + entityName);
    				}
    			}
    		}
    	}
    	setAttributes(list.toArray(new XAttributeImpl[0]));    	
    }

    private void loadDependencies(Element el) {
        Element c = XMetaDataLoader.getUniqueChild(el, "XDependencies");
        XDependencies d = (c == null) ? null : (XDependencies)XMetaDataLoader.loadMetaElement(c, null, false);
        if(d != null) dependencies = d;
    }

    public String toString(){
        StringBuffer sb = new StringBuffer(100);
        sb.append(getName());
        sb.append('\n');
        for(int i = 0; i < m_Attributes.length;++i)
          sb.append(" " + m_Attributes[i] + "\n");
        return sb.toString();
    }

    ///////// dependencies

    public boolean isVisible(XModelObject object, String attribute) {
        return dependencies.isVisible(object, attribute);
    }

    public boolean isEditable(XModelObject object, String attribute) {
        return dependencies.isEditable(object, attribute);
    }

    public void setDependentValues(XModelObject object, String attribute) {
        dependencies.setDependentValues(object, attribute);
    }

    //// adopt

    public XAdoptManager getAdoptManager() {
        return adopt;
    }
    
    public void setAdoptManager(String adoptclass) {
        try {
        	if(adoptclass != null && adoptclass.length() > 0) {
        		adopt = (XAdoptManager)ModelFeatureFactory.getInstance().createFeatureInstance(adoptclass);
        	}
        } catch (Exception e) {
        	ModelPlugin.log("XModelEntityImpl:setAdoptManager:" + e.getMessage());
        }
    }

    void validateChildren() {
        children.validate(getMetaModel());
    }

    //// merge

    private void merge(XModelEntityExtensionImpl ext) {
        children.merge(ext.getChildren());
        try {
            actions.merge((XActionListImpl)ext.getActionList());
        } catch (Exception e) {
        	ModelPlugin.log("XModelEntityImpl:merge:" + e.getMessage());
        }
    }

    //optimization

    private HashMap<String,Integer> registered = new HashMap<String,Integer>();

    public int getPropertyIndex(String name, boolean register) {
        Integer io = registered.get(name);
        if(io == null) {
            if(!register) return -1;
            synchronized (registered) {
            	io = registered.get(name);
            	if(io == null) {
            		io = new Integer(registered.size());
            		registered.put(name, io);
            	}
			}
        }
        return io.intValue();
    }

    public int getPropertyCount() {
        return registered.size();
    }

    private Properties xmlmap = null;
    
    static Set<String> unfoundEntities = null;

    public String getChildByXML(String xmlname) {
        if(xmlmap == null) {
            xmlmap = new Properties();
            XChild[] cs = children.getChildren();
            for (int i = 0; i < cs.length; i++) {
                String n = cs[i].getName();
                XModelEntity e = getMetaModel().getEntity(n);
                if(e == null) {
                	if(unfoundEntities == null) unfoundEntities = new HashSet<String>();
                	if(!unfoundEntities.contains(n)) {
                		unfoundEntities.add(n);
                		ModelPlugin.log("getChildByXML: cannot find child entity " + n);
                	}
                	continue;
                }
                String x = e.getXMLSubPath();
                if(x == null || x.length() == 0) continue;
                xmlmap.setProperty(x, n);
            }
        }
        return xmlmap.getProperty(xmlname);
    }

    private HashSet<String> requiredchildren = null;
    private boolean requiredloaded = false;

    public java.util.HashSet<String> getRequiredChildren() {
        if(!requiredloaded) {
            XChild[] cs = children.getChildren();
            requiredchildren = new HashSet<String>(cs.length);
            for (int i = 0; i < cs.length; i++) {
                boolean required = (cs[i].getMaxCount() == 1 && cs[i].isRequired());
                if(required) requiredchildren.add(cs[i].getName());
            }
            if(requiredchildren.size() == 0) requiredchildren = null;
            requiredloaded = true;
        }
        return (requiredchildren == null) ? null : (HashSet<String>)requiredchildren.clone();
    }
    
    public String testImplementation() {
    	if(implementationClassName == null || implementationClassName.length() == 0) {
    		return null;
    	}
    	String cn = expand(implementationClassName, "Implementations");
    	if(cn == null) {
    		return "cannot expand implementation " + implementationClassName;
    	}
    	Class cls = ModelFeatureFactory.getInstance().getFeatureClass(cn);
    	if(cls == null) {
    		return "cannot load implementation class " + cn;
    	}
    	try {
    		Object h = cls.newInstance();
    		if(!(h instanceof XModelObject)) {
    			return "cannot reduce implementation to XModelObject";
    		}
    	} catch (Exception e) {
    		return "cannot create implementation object";
    	}
    	return null;
    }

    public String testLoader() {
    	if(loaderClassName == null || loaderClassName.length() == 0) {
    		return null;
    	}
    	String cn = expand(loaderClassName, "Loaders");
    	if(cn == null) {
    		return "cannot expand loader " + loaderClassName;
    	}
    	Class cls = ModelFeatureFactory.getInstance().getFeatureClass(cn);
    	if(cls == null) {
    		return "cannot load loader class " + cn;
    	}
    	try {
    		Object h = cls.newInstance();
    		if(!(h instanceof XObjectLoader)) {
    			return "cannot reduce loader to XObjectLoader";
    		}
    	} catch (Exception e) {
    		return "cannot create loader object";
    	}
    	return null;
    }

}

class ClassHolder {
    private String name = null;
    private XMetaElementImpl element = null;
    private String map = null;

    public ClassHolder(String name, XMetaElementImpl element, String map) {
        this.name = name;
        this.element = element;
        this.map = map;
    }
    
    public Class getHoldedClass() {
    	if(name == null) return null;
    	validate();
    	try {
    		Class c = ModelFeatureFactory.getInstance().getFeatureClass(name);
    		if(c == null) name = null;
    		return c;
    	} catch (Exception e) {
    		name = null;
    		return null;
    	}
    }

    private void validate() {
        if(name == null) return;
        if(map != null && name.startsWith("%")) {
            name = element.expand(name, map);
        }
		map = null;
    }

	public String getProperty(String name) {
		return null;
	}
}

