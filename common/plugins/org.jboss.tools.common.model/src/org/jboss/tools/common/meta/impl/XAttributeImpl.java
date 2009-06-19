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

import org.w3c.dom.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.constraint.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.meta.constraint.impl.*;

public class XAttributeImpl extends XMetaElementImpl implements XAttribute {
	private XModelEntity entity;
    protected XAttributeEditor m_Editor;
    protected String m_DefValue;
    protected boolean m_Visible;
    protected ConstraintHolder constraint = new ConstraintHolder();
    protected boolean m_Required;
    protected boolean m_Editable;
    protected boolean trimmable;
    protected boolean copyable;
    protected String m_XMLName;
    protected AdapterHolder adapter = new AdapterHolder();

    public XAttributeImpl() {}
    
    public XAttributeImpl copy() {
    	XAttributeImpl copy = new XAttributeImpl();
    	copy.name = name;
    	copy.displayName = displayName;
    	copy.p = p;
    	copy.m_Editor = m_Editor;
    	copy.m_DefValue = m_DefValue;
    	copy.m_Visible = m_Visible;
    	copy.constraint = constraint;
    	copy.m_Required = m_Required;
    	copy.m_Editable = m_Editable;
    	copy.trimmable = trimmable;
    	copy.copyable = copyable;
    	copy.m_XMLName = m_XMLName;
    	copy.adapter = adapter;
    	return copy;
    }

    public void valueChanged(XModelObject object) {
////        for (int i = 0; i < valuelisteners.length; i++)
////          valuelisteners[i].valueChanged(object, getName());
    }

	public XModelEntity getModelEntity() {
		return entity;
	}
	
	void setModelEntity(XModelEntity entity) {
		this.entity = entity;
	}
	
    public XAttributeEditor getEditor() {
        return m_Editor;
    }

    void setEditor(XAttributeEditor ed) {
        m_Editor = ed;
    }

    public String getDefaultValue() {
        return m_DefValue;
    }

    public void setDefaultValue(String value) {
        m_DefValue = replace(value, "%cr%", "\n");  //$NON-NLS-1$//$NON-NLS-2$
    }

    public boolean isTrimmable() {
        return trimmable;
    }

    public boolean isCopyable() {
        return copyable;
    }

    public boolean isVisible() {
        return m_Visible;
    }

    void setVisible(boolean v) {
        m_Visible = v;
    }

    public XAttributeConstraint getConstraint() {
        return constraint.getConstraint();
    }

    public boolean isRequired() {
        return m_Required;
    }

    void setRequired(boolean req) {
        m_Required = req;
    }

    public boolean isEditable() {
        return m_Editable;
    }

    void setEditable(boolean ed) {
        m_Editable = ed;
    }

    void setXMLName(String xname) {
        m_XMLName = xname;
    }

    public String getXMLName() {
        return m_XMLName;
    }

    public boolean isFake() {
        return "FAKE".equals(m_XMLName); //$NON-NLS-1$
    }

    public void load(Element el){
    	if(XMLUtil.getUniqueChild(el, EDITOR) == null) {
    		XAttributeEditorImpl editor = new XAttributeEditorImpl();
    		editor.setName(null);
    		setEditor(editor);
    	} else {
    		setEditor((XAttributeEditor)XMetaDataLoader.loadMetaElement(el, EDITOR, XAttributeEditorImpl.class, false));
    	}
        Element c = XMetaDataLoader.getUniqueChild(el, CONSTRAINT);
        if (c != null) loadConstraint(c);
        setVisible(XMetaDataLoader.getBoolean(el, VISIBLE, true));
        setRequired(XMetaDataLoader.getBoolean(el, REQUIRED, false));
        setEditable(XMetaDataLoader.getBoolean(el, EDITABLE, false));
        setDefaultValue(el.getAttribute(DEFAULT_VALUE));
        setXMLName(el.getAttribute(XML_NAME));
        loadAdapter(el);
        trimmable = !XModelObjectConstants.NO.equals(el.getAttribute("TRIM"));  //$NON-NLS-1$//$NON-NLS-2$
        copyable = !XModelObjectConstants.NO.equals(el.getAttribute("COPYABLE"));  //$NON-NLS-1$//$NON-NLS-2$
        loadProperties(el);
    }

    public String toString(){
        return getName();
    }

    public static final String LOADER = "loader"; //$NON-NLS-1$
    public static final String CONSTRAINT_PREFIX =
       "org.jboss.tools.common.meta.constraint.impl.XAttributeConstraint"; //$NON-NLS-1$
    public static final String ATTRIBUTE_PREFIX =
       "org.jboss.tools.common.meta.impl.adapters.XAdapter"; //$NON-NLS-1$

    public void loadConstraint(Element element) {
        if(element == null) return;
        String loader = element.getAttribute(LOADER);
        if(!XMetaDataLoader.hasAttribute(element, LOADER) || loader.trim().length() == 0) {
        	loader = ""; //$NON-NLS-1$
        }
        if(loader != null && loader.startsWith("%")) { //$NON-NLS-1$
        	loader = expand(loader, "AttributeConstraints"); //$NON-NLS-1$
        }
        constraint.init(loader, element);
    }

    public void loadAdapter(Element element) {
        String loader = element.getAttribute(LOADER);
        if(loader.length() == 0 && getXMLName().length() > 0) loader = "ModelElement"; //$NON-NLS-1$
        adapter.init(this, loader, element);
        loadValueListeners(element);
    }

    public XAdapter getAdapter() {
        return adapter.getAdapter();
    }

    public void loadValueListeners(Element element) {
    }

    private static String replace(String source, String a, String b) {
        if(source == null || source.indexOf(a) < 0) return source;
        StringBuffer sb = new StringBuffer();
        int i = 0, sL = source.length(), aL = a.length();
        while(i < sL) {
            int j = source.indexOf(a, i);
            if(j < 0) {
                sb.append(source.substring(i));
                break;
            }
            sb.append(source.substring(i, j)).append(b);
            i = j + aL;
        }
        return sb.toString();
    }

}

class AdapterHolder {
	private XAttributeImpl attribute;
	private XAdapter adapter;
	private String loader;
	private Element element;
	
	public AdapterHolder() {}

	public void init(XAttributeImpl attribute, String loader, Element element) {
		this.attribute = attribute;
		this.loader = loader;
		this.element = element;
		adapter = null;
	}
	
	public XAdapter getAdapter() {
		if(adapter != null) return adapter;
			doGetAdapter();
		if(adapter == null) adapter = new XAdapter();
		return adapter;
	}

	private synchronized void doGetAdapter() {
		String loader = this.loader;
		Element element = this.element;
		if(this.adapter != null) return;
		XAdapter adapter = null;
        if(loader == null || loader.length() == 0) {
            adapter = new XAdapter();
        } else {
        	String clsname = loader.indexOf('.') >= 0 ? loader : XAttributeImpl.ATTRIBUTE_PREFIX + loader;
            try {
                if(loader.length() > 0) {
                	adapter = (XAdapter)ModelFeatureFactory.getInstance().createFeatureInstance(clsname);
                }
            } catch (ClassCastException e) {
            	ModelPlugin.getPluginLog().logError("XAttributeImpl:loadAdapter:" + e.getMessage()); //$NON-NLS-1$
            }
        }
		if(adapter == null) adapter = new XAdapter();
        if(adapter != null) {
        	adapter.reload(element);
            adapter.setConstraint(attribute.getConstraint());
        }
		
		loader = null;
		element = null;
		this.adapter = adapter;
	}

}

class ConstraintHolder {
	private XAttributeConstraint constraint;
	private String loader;
	private Element element;
	
	public ConstraintHolder() {}

	public void init(String loader, Element element) {
		this.loader = loader;
		this.element = element;
		constraint = null;
	}
	
	public XAttributeConstraint getConstraint() {
		if(constraint == null) {
				doGetConstraint();
			if(constraint == null) constraint = new XAttributeConstraintImpl();
		}
		return constraint;
	}
	
	private void doGetConstraint() {
		String loader = this.loader;
		Element element = this.element;
		if(this.constraint != null) return;
		XAttributeConstraint constraint = null;
        if(loader != null) try {
        	String clsname = loader.indexOf('.') >= 0 ? loader : XAttributeImpl.CONSTRAINT_PREFIX + loader;
            if(loader.length() > 0) {
            	constraint = (XAttributeConstraint)ModelFeatureFactory.getInstance().createFeatureInstance(clsname);
            }
        } catch (ClassCastException e) {
        	ModelPlugin.getPluginLog().logError("XAttributeImpl:loadConstraint:" + e.getMessage(), e); //$NON-NLS-1$
        }
		if(constraint == null) constraint = new XAttributeConstraintImpl();
		
		if(element != null) 
			((XAttributeConstraintImpl)constraint).load(element);
		loader = null;
		element = null;
		this.constraint = constraint;
	}

}
