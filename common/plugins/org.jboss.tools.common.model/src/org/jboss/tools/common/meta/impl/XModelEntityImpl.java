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

public class XModelEntityImpl extends XMetaElementImpl implements XModelEntity {
	public static boolean hideHelp = true;
	protected String module;
    protected XChildrenImpl children = new XChildrenImpl();
    protected XAttribute[] m_Attributes;
    protected XActionListImpl actions = new XActionListImpl();
    protected XEntityRenderer m_Renderer = null;
    protected String implementationClassName = null;
    protected String resolvedImplementationClassName = null;
    protected String m_GeneratorClassName = null;
    protected String loaderClassName = null;
    protected String resolvedLoaderClassName = null;
    protected String m_EditorClassName = null;
    protected XDependencies dependencies = new DefaultDependencies();
    protected XAdoptManager adopt = null;

    protected String xmlSubPath = null;
    protected Element element = null;

    protected XModelEntityImpl() {}

    public void setModule(String s) {
    	module = s;
    }

    public String getModule() {
    	return module;
    }

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
            ArrayList<XModelEntityExtensionImpl> es = meta.getExtensions().getExtensions(getName());
            if(es != null) {
                for (int i = 0; i < es.size(); i++)
                  merge((XModelEntityExtensionImpl)es.get(i));
                if(!hideHelp) addHelpAction();
            }
        }
    }
    
    private void addHelpAction() {
    	XActionListImpl list = actions;
		XActionItem item = actions.getItem("Properties"); //$NON-NLS-1$
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
    
    public Class getImplementingClass() {
    	if(resolvedImplementationClassName == UNRESOLVED) {
    		resolvedImplementationClassName = expand(implementationClassName, "Implementations"); //$NON-NLS-1$
    	}
    	if(resolvedImplementationClassName == null) return null;
    	Class cls = ModelFeatureFactory.getInstance().getFeatureClass(resolvedImplementationClassName);
    	if(cls == null) {
    		resolvedImplementationClassName = null;
    	}
        return cls;
    }

    public boolean hasObjectImplementation() {
    	return implementationClassName != null && implementationClassName.length() > 0;
    }

    public XModelObject getObjectImplementation() {
    	if(resolvedImplementationClassName == UNRESOLVED) {
    		resolvedImplementationClassName = expand(implementationClassName, "Implementations"); //$NON-NLS-1$
    	}
    	if(resolvedImplementationClassName == null) return null;
    	XModelObject o = ModelFeatureFactory.getInstance().createXModelObjectInstance(resolvedImplementationClassName);
    	if(o == null) {
    		resolvedImplementationClassName = null;
    	}
    	return o;
    }

    public void setImplementingClassName(String className){
       if(className != null && className.length() == 0) className = null;
       implementationClassName = className;
       if(className != null) {
    	   resolvedImplementationClassName = UNRESOLVED;
       }
    }

    public boolean hasObjectLoader() {
    	return loaderClassName != null && loaderClassName.length() > 0;
    }

    public XObjectLoader getObjectLoader() {
    	if(resolvedLoaderClassName == UNRESOLVED) {
    		resolvedLoaderClassName = expand(loaderClassName, "Loaders"); //$NON-NLS-1$
    	}
    	if(resolvedLoaderClassName == null) return null;
    	Object o = ModelFeatureFactory.getInstance().createFeatureInstance(resolvedLoaderClassName);
    	if(o == null) {
    		return null;
    	}
    	if(!(o instanceof XObjectLoader)) {
    		ModelPlugin.getPluginLog().logError("Model object loader" + resolvedLoaderClassName + " must implement " + XObjectLoader.class.getName()); //$NON-NLS-1$ //$NON-NLS-2$
    		resolvedLoaderClassName = null;
    		return null;
    	}
        return (XObjectLoader)o;
    }
    
    static String UNRESOLVED = "UNRESOLVED"; //$NON-NLS-1$

    public void setLoaderClassName(String className) {
       if(className != null && className.length() > 0) {
         loaderClassName = className;
         resolvedLoaderClassName = UNRESOLVED;
       }
    }

    public String getEditorClassName() {
        return (m_EditorClassName = expand(m_EditorClassName, "ObjectEditor")); //$NON-NLS-1$
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
        xmlSubPath = element.getAttribute("XMLSUBPATH"); //$NON-NLS-1$
        Element ei = XMetaDataLoader.getUniqueChild(element, "XActionItem"); //$NON-NLS-1$
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
    				String entityName = e.getAttribute(XMetaDataConstants.ENTITY);
    				String attrName = e.getAttribute(NAME);
    				String attributes = e.getAttribute("attributes"); //$NON-NLS-1$
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
        					} else if(ModelPlugin.isDebugEnabled()) {
        						ModelPlugin.getPluginLog().logInfo("Cannot find reference to attribute " + attrName + " of entity " + entityName); //$NON-NLS-1$ //$NON-NLS-2$
        					}
    					}
    				} else if(ModelPlugin.isDebugEnabled()) {
    					ModelPlugin.getPluginLog().logInfo("Cannot find reference to entity " + entityName); //$NON-NLS-1$
    				}
    			}
    		}
    	}
    	setAttributes(list.toArray(new XAttributeImpl[0]));    	
    }

    private void loadDependencies(Element el) {
        Element c = XMetaDataLoader.getUniqueChild(el, "XDependencies"); //$NON-NLS-1$
        XDependencies d = (c == null) ? null : (XDependencies)XMetaDataLoader.loadMetaElement(c, null, false);
        if(d != null) dependencies = d;
    }

    public String toString(){
        StringBuffer sb = new StringBuffer(100);
        sb.append(getName());
        sb.append('\n');
        for(int i = 0; i < m_Attributes.length;++i)
          sb.append(" " + m_Attributes[i] + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
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
       	if(adoptclass != null && adoptclass.length() > 0) {
       		adopt = new XAdoptWrapper(adoptclass, this);
       	}
    }

    void validateChildren() {
        children.validate(getMetaModel());
    }

    //// merge

    private void merge(XModelEntityExtensionImpl ext) {
        children.merge(ext.getChildren());
        actions.merge((XActionListImpl)ext.getActionList());
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
            		io = Integer.valueOf(registered.size());
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
                		ModelPlugin.getPluginLog().logInfo("getChildByXML: cannot find child entity " + n); //$NON-NLS-1$
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
    
    
    /**
     * FIXME Move to ModelTest plugin
     */
    public String testImplementation() {
    	if(implementationClassName == null || implementationClassName.length() == 0) {
    		return null;
    	}
    	String cn = expand(implementationClassName, "Implementations"); //$NON-NLS-1$
    	if(cn == null) {
    		return "cannot expand implementation " + implementationClassName; //$NON-NLS-1$
    	}
    	Class cls = ModelFeatureFactory.getInstance().getFeatureClass(cn);
    	if(cls == null) {
    		return "cannot load implementation class " + cn; //$NON-NLS-1$
    	}
    	try {
    		Object h = cls.newInstance();
    		if(!(h instanceof XModelObject)) {
    			return "cannot reduce implementation to XModelObject"; //$NON-NLS-1$
    		}
    	} catch (InstantiationException e) {
    		return "cannot create implementation object"; //$NON-NLS-1$
		} catch (IllegalAccessException e) {
			return "cannot create implementation object"; //$NON-NLS-1$
		}
    	return null;
    }

    
    /**
     * FIXME Move to ModelTest plugin
     */
    public String testLoader() {
    	if(loaderClassName == null || loaderClassName.length() == 0) {
    		return null;
    	}
    	String cn = expand(loaderClassName, "Loaders"); //$NON-NLS-1$
    	if(cn == null) {
    		return "cannot expand loader " + loaderClassName; //$NON-NLS-1$
    	}
    	Class cls = ModelFeatureFactory.getInstance().getFeatureClass(cn);
    	if(cls == null) {
    		return "cannot load loader class " + cn; //$NON-NLS-1$
    	}
    	try {
    		Object h = cls.newInstance();
    		if(!(h instanceof XObjectLoader)) {
    			return "cannot reduce loader to XObjectLoader"; //$NON-NLS-1$
    		}
    	} catch (InstantiationException e) {
    		return "cannot create loader object"; //$NON-NLS-1$
		} catch (IllegalAccessException e) {
			return "cannot create loader object"; //$NON-NLS-1$
		}
    	return null;
    }

}

class XAdoptWrapper implements XAdoptManager {
	String adoptclass;
	XModelEntityImpl entity;
	
	public XAdoptWrapper(String adoptclass, XModelEntityImpl entity) {
		this.adoptclass = adoptclass;
		this.entity = entity;
	}

	public void adopt(XModelObject target, XModelObject object, Properties p) throws XModelException {
		validate();
		if(entity.adopt != null) {
			entity.adopt.adopt(target, object, p);
		}
	}

	public boolean isAdoptable(XModelObject target, XModelObject object) {
		validate();
		return entity.adopt != null && entity.adopt.isAdoptable(target, object);
	}

	void validate() {
	    try {
	    	if(adoptclass != null && adoptclass.length() > 0) {
	    		entity.adopt = (XAdoptManager)ModelFeatureFactory.getInstance().createFeatureInstance(adoptclass);
	    	}
	    } catch (ClassCastException e) {
	    	ModelPlugin.getPluginLog().logError("XModelEntityImpl:setAdoptManager:" + e.getMessage()); //$NON-NLS-1$
	    	entity.adopt = null;
	    }
	}

}
