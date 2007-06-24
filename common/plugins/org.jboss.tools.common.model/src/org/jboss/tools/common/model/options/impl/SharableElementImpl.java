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
package org.jboss.tools.common.model.options.impl;

import java.util.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.options.*;

public class SharableElementImpl extends XModelObjectImpl implements SharableElement {
    private static final long serialVersionUID = 7922861006119791921L;

    private static final Map<String,String> singular = new HashMap<String,String>(3);
    static {
        singular.put("scope", "scope");
        singular.put("sharing", "sharing");
    }

    static boolean isSingular(String property) {
        return singular.get(property) != null;
    }

    protected XScope general_ = new XScope(GENERAL);
    protected XScope project_ = new XScope(PROJECT);
    protected Map<String,XScope> scopes = new HashMap<String,XScope>();
    protected XScope scope = project_;
    private Map<String,SharableElement> children = new HashMap<String,SharableElement>();
    private SharableComparator comparator = null;
    protected String name = "";

    public SharableElementImpl() {
        super();
        scopes.put(GENERAL, general_);
        scopes.put(PROJECT, project_);
    }

    public SharableElement getSharableParent() {
        Object p = getParent();
        return (p != null && p instanceof SharableElement)
               ? (SharableElement)p : null;
    }

    public SharableElement[] getSharableChildren() {
        return children.values().toArray(new SharableElement[0]);
    }
    
    private int getChildrenCount(String entity) {
		SharableElement[] cs = getSharableChildren();
		int k = 0; 
		for (int i = 0; i < cs.length; i++) {
			if(entity.equals(cs[i].getModelEntity().getName())) ++k;
		}
		return k;
    }

    private void setScope0(String scopename) {
        scope = (XScope)scopes.get(scopename);
        if(scope == null) scope = project_;
    }

    public void setScope(String scopename) {
        setScope0(scopename);
        if(!scope.exists()) scope.setExists(true);
    }

    public String getScope() {
        return scope.getName();
    }

    public boolean exists() {
        return general_.exists() || project_.exists();
    }

    XScope getXScope(String scopename) {
         return (XScope)scopes.get(scopename);
    }

    public boolean scopeExists(String scopename) {
        XScope s = getXScope(scopename);
        return (s != null && s.exists());
    }

    public void setScopeExists(String scopename, boolean b) {
        XScope s = getXScope(scopename);
        if(s == null || s.exists() == b ||
           !XStudioLoaderPeer.instance().isScopeEditable(scopename)) return;
        setScopeExists0(scopename, b);
    }

    void setScopeExists0(String scopename, boolean b) {
        XScope s = getXScope(scopename);
        if(s == null || s.exists() == b) return;
        s.setExists(b);
        if(!b) {
            SharableElement[] cs = getSharableChildren();
            for (int i = 0; i < cs.length; i++) {
                SharableElementImpl c = (SharableElementImpl)cs[i];
              if(c != null) c.setScopeExists0(scopename, false);
            }
        } else {
            SharableElementImpl p = (SharableElementImpl)getSharableParent();
            if(p != null) p.setScopeExists0(scopename, true);
        }
    }

    protected void onSetEntity(String entityName) {
        XModelEntity entity = getModel().getMetaData().getEntity(entityName);
        XAttribute[] as = entity.getAttributes();
        for (int i = 0; i < LIST.length; i++) {
            scope = (XScope)scopes.get(LIST[i]);
            for (int j = 0; j < as.length; j++) {
                if(isSingular(as[j].getName())) continue;
                setAttributeValue(as[j].getName(), as[j].getDefaultValue());
            }
        }
        comparator = new SharableComparator(entity.getChildren());
    }

    ////////////////// children ////////////////
    public boolean addSharableChild(SharableElement child) {
        String childname = child.name();
        if(findSharableChild(childname) != null) return false;
        children.put(childname, child);
        return true;
    }

    public void removeSharableChild(String name) {
        SharableElementImpl s = (SharableElementImpl)children.get(name);
        if(s == null) return;
        for (int i = 0; i < LIST.length; i++) {
            XScope sc = getXScope(LIST[i]);
            if(sc.exists() &&
               !XStudioLoaderPeer.instance().isScopeEditable(LIST[i]))
              return;
        }
        children.remove(name);
    }

    public SharableElement findSharableChild(String childname) {
        return (SharableElement)children.get(childname);
    }

    ////////////////// override ////////////////
    public String get(String name) {
        if("scope".equals(name)) {
            return getScope();
        } else if("sharing".equals(name)) {
            String x = (general_.exists() ? GENERAL + "," : "")
                     + (project_.exists() ? PROJECT + "," : "");
            int c = x.length() - 1;
            return (c < 0) ? "" : x.substring(0, c);
        }
        return ("NAME".equals(name)) ? name()
               : scope.getProperty(name);
    }

    public void set(String name, String value) {
        if("scope".equals(name)) {
            String ov = get(name);
            if(ov.equals(value)) return;
            String max = XStudioLoaderPeer.getMaxScope(ov, value);
            if(!XStudioLoaderPeer.instance().isScopeEditable(max)) return;
            setScope0(value);
            if(!scope.exists())
              setScopeExists(scope.getName(), true);
        } else if("sharing".equals(name)) {
            setScopeExists(GENERAL, value.indexOf(GENERAL) >= 0);
            setScopeExists(PROJECT, value.indexOf(PROJECT) >= 0);
        } else if("NAME".equals(name)) {
            setName(value);
        } else {
            scope.setProperty(name, value);
        }
    }

    public String getAttributeValue(String name, String scopename) {
        XScope current = scope;
        setScope0(scopename);
        String res = getAttributeValue(name);
        scope = current;
        return res;
    }
    
    public String getDefaultValue(String name) {
    	XAttribute a = getModelEntity().getAttribute(name);
    	if(a == null) return null;
    	if(a.getXMLName() == null || a.getXMLName().length() == 0) return a.getDefaultValue();
    	String v = getAttributeValue(name, GENERAL);
    	if(v == null || v.length() == 0) v = a.getDefaultValue();
    	return v;
    }

    protected String getEntityName() {
        return "SharableDefault";
    }

    public String name() {
        return name;
    }
    public void setName_0(String value) {
        name = value;
    }

    public boolean setName(String value) {
        if(value == null) return false;
        String oldvalue = name();
        if(value.equals(oldvalue)) return true;
        SharableElement p = getSharableParent();
        if(p != null && p.findSharableChild(value) != null) return false;
        if(p != null && p.findSharableChild(oldvalue) == null) return false;
        String op = getPath();
        setName_0(value);
        if(p != null) {
            SharableElementImpl sp = (SharableElementImpl)p;
            sp.children.remove(oldvalue);
            sp.children.put(value, this);
            ((XModelImpl)getModel()).fireNodeChanged(this, op);
            fireUpdateChilds();
        }
        return true;
    }

    protected void fireUpdateChilds() {}

    public boolean addChild_0(XModelObject child) {
        if(child == null) return false;
        boolean res = false;
        String ce = child.getModelEntity().getName();
        XChild c_ent = getModelEntity().getChild(ce);
        if(c_ent == null) return false;
		if(c_ent.getMaxCount() < Integer.MAX_VALUE && getChildrenCount(ce) >= c_ent.getMaxCount()) {
			return false;
		}
        if(child instanceof SharableElement) {
            SharableElementImpl sc = (SharableElementImpl)child;
            res = addSharableChild(sc);
            if(res) {
                sc.setParent_0(this);
                for (int i = 0; i < LIST.length; i++) {
                    if(sc.scopeExists(LIST[i])) {
                        setScopeExists(LIST[i], true);
                    }
                    if(!scopeExists(LIST[i])) {
                        ((XScope)sc.scopes.get(LIST[i])).setExists(false);
                    }
                }
            }
        }
        return res;
    }

    public void removeChild_0(XModelObject o) {
        if(o instanceof SharableElement) {
            removeSharableChild(((SharableElement)o).name());
        }
    }
    
    public XModelObject[] getChildren() {
        SharableElement[] cs = getSharableChildren();
        Arrays.sort(cs, comparator);
        return cs;
    }

    public boolean isObjectEditable() {
        return isScopeEditable(scope.getName());
    }

    public boolean isScopeEditable(String scopename) {
        return XStudioLoaderPeer.instance().isScopeEditable(scopename);
    }

    public boolean isScopeExists(String scopename) {
        XScope s = getXScope(scopename);
        return (s != null && s.exists());
    }

    public boolean isAttributeEditable(String attributeName) {
        return isSingular(attributeName) || isObjectEditable();
    }

    private void shareProperties0(XScope fs, XScope ts) {
        Properties fp = fs.properties(), tp = ts.properties();
        Enumeration en = fp.keys();
        while(en.hasMoreElements()) {
            String key = (String)en.nextElement();
            String val = fp.getProperty(key);
            tp.setProperty(key, val);
        }
    }

    public void sharing(String fromscope, String toscope) {
        XScope fs = getXScope(fromscope), ts = getXScope(toscope);
        if(!ts.exists()) setScopeExists(toscope, true);
        if(!ts.exists()) return;
        shareProperties0(fs, ts);
        childsharing(fromscope, toscope);
    }

    protected void childsharing(String fromscope, String toscope) {
        ((XModelImpl)getModel()).fireNodeChanged(this, getPath());
    }

    public void merge(String fromscope, String toscope, boolean merge_all) {
        XScope fs = getXScope(fromscope), ts = getXScope(toscope);
        boolean existed = ts.exists();
        if(!existed) setScopeExists(toscope, true);
        if(!ts.exists()) return;
        if(!existed || merge_all) shareProperties0(fs, ts);
        mergeChildren(fromscope, toscope, existed, merge_all);
        setScope(PROJECT);
    }

    protected void mergeChildren(String fromscope, String toscope, boolean existed, boolean merge_all) {
        SharableElement[] cs = getSharableChildren();
        for (int i = 0; i < cs.length; i++)
          ((SharableElementImpl)cs[i]).merge(fromscope, toscope, merge_all);
    }

}

class XScope {
    private String name;
    private Properties properties = new Properties();
    private XModelObject child = null;
    private boolean exists = false;
    public XScope(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public String getProperty(String name) {
        return properties.getProperty(name);
    }

    public void setProperty(String name, String value) {
        properties.setProperty(name, value);
    }

    public boolean exists() {
        return exists;
    }

    public void setExists(boolean b) {
        exists = b;
    }

    public XModelObject getChild() {
        return child;
    }

    public void setChild(XModelObject child) {
        this.child = child;
    }

    Properties properties() {
        return properties;
    }

}

class SharableComparator implements Comparator<SharableElement> {
    private Hashtable<String,Integer> entities = null;

    public SharableComparator(XChild[] c) {
        entities = new Hashtable<String,Integer>(c.length);
        for (int i = 0; i < c.length; i++) {
            entities.put(c[i].getName(), new Integer(i * 100));
        }
    }

    public int compare(SharableElement o1, SharableElement o2) {
        int i1 = getEntityRange(o1);
        int i2 = getEntityRange(o2);
        if(i1 != i2) return (i1 - i2) ;
        return o1.name().compareTo(o2.name());
    }

    private int getEntityRange(SharableElement s) {
        Integer i = entities.get(s.getModelEntity().getName());
        return (i == null) ? 1000 : i.intValue();
    }

    public boolean equals(Object obj) {
        return obj == this;
    }

}

