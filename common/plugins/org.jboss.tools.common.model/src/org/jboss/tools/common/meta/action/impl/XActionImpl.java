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
package org.jboss.tools.common.meta.action.impl;

import java.util.Properties;

import org.w3c.dom.Element;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultSpecialHandler;
import org.jboss.tools.common.meta.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;

public class XActionImpl extends XActionItemImpl implements XAction {
    private String wizard;
    private String handlerClassName = null;
    private ClassHolder2 handlername = null;
    private XEntityData[] data;
    private AbstractHandler handler = new AbstractHandler();
    private String baseaction = null;
    private boolean isSave2ModelRequired = false;
    private String hide = HIDE_NEVER;

    public XActionImpl() {}

    public String getWizardClassName() {
        return (wizard = expand(wizard, "Wizards"));
    }

    public XRedirect getRedirect() {
    	return handler instanceof XRedirect ? (XRedirect)handler : null; 
    }
    
    public String getBaseActionName() {
        return baseaction;
    }

    public boolean isSave2ModelRequired() {
        return isSave2ModelRequired;
    }

    public boolean hide(boolean enabled) {
        return (!hasHandler()) ? hide0(enabled) : handler.hide(enabled);
    }

    public XEntityData[] getEntityData(XModelObject object) {
        data = (!hasHandler()) ? new XEntityData[0] : handler.getEntityData(object);
        return data;
    }

    public void setDefaultData(XModelObject object) {
        if(hasHandler()) handler.setDefaultData(object);
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(hasHandler()) handler.executeHandler(object, p);
    }

    public boolean getSignificantFlag(XModelObject object) {
        return hasHandler() && handler.getSignificantFlag(object);
    }

    public boolean isEnabled(XModelObject object) {
        return hasHandler() && handler.isEnabled(object);
    }

    public boolean isEnabled(XModelObject object, XModelObject[] objects) {
        return hasHandler() && handler.isEnabled(object, objects);
    }

    public void executeHandler(XModelObject object, XModelObject[] objects, java.util.Properties p) throws Exception {
        if(hasHandler()) handler.executeHandler(object, objects, p);
    }

    private boolean hasHandler() {
        if(handlername != null) {
            handler = (AbstractHandler)handlername.createInstance();
            if(handler == null) handler = new AbstractHandler();
            handler.setAction(this);
            handler.setData(data);
            handlername = null;
        }
        return (handler != null);
    }

    public void load(Element el) {
        super.load(el);
        handlerClassName = el.getAttribute("HandlerClassName");
        handlername = new ClassHolder2(this, el.getAttribute("HandlerClassName"), "Handlers");
        wizard = el.getAttribute("WizardClassName");
        baseaction = (!XMetaDataLoader.hasAttribute(el, "BaseActionName")) ? null :
                       el.getAttribute("BaseActionName");
        isSave2ModelRequired = "yes".equals(el.getAttribute("SAVE_REQUIRED"));
        hide = el.getAttribute("HIDE");
        Element[] cs = XMetaDataLoader.getChildrenElements(el, "EntityData");
        data = new XEntityDataImpl[cs.length];
        for (int i = 0; i < cs.length; i++) {
            data[i] = new XEntityDataImpl();
            ((XEntityDataImpl)data[i]).load(cs[i]);
        }
        String callbackname = el.getAttribute("CallBackClassName");
        if(callbackname != null && callbackname.length() > 0) {
        	ModelPlugin.getPluginLog().logInfo("Warning: callback=" + callbackname);
        }
    }

    boolean hide0(boolean enabled) {
        return hide.equals(HIDE_ALWAYS) || (!enabled && hide.equals(HIDE_DISABLED));
    }

    public XActionItem copy(XActionItem.Acceptor acceptor) {
        return (acceptor.accepts(this)) ? this : null;
    }

    // dirty hacks

    /*
     * Should be called only through AbstractHandler
     */

    void setWizardClassName(String wizard) {
        this.wizard = wizard;
    }

    /*
     * universal help action
     */

    private static XAction help = null;

    public static XAction getHelpAction() {
        if(help != null) return help;
        XActionImpl a = new XActionImpl();
        a.handlername = new ClassHolder2(a, "org.jboss.tools.common.meta.help.HelpHandler", "Handlers");
        a.setDisplayName("Help");
        a.setName("Help");
        a.iconname = "action.empty";
        a.data = new XEntityDataImpl[0];
        return help = a;
    }
    
    public String testHandler() {
    	if(handlerClassName == null || handlerClassName.trim().length() == 0) {
    		return null; //"handler is not set";
    	}
    	String cn = expand(handlerClassName, "Handlers");
    	if(cn == null) {
    		return "cannot expand handler name " + handlerClassName;
    	}
    	Class cls = ModelFeatureFactory.getInstance().getFeatureClass(cn);
    	if(cls == null) {
    		return "cannot load handler class " + cn;
    	}
    	Object h = null;
    	try {
    		h = cls.newInstance();
    	} catch (NoClassDefFoundError nc) {
    		//just wrong class path in test plugin
    		return null;
    	} catch (Exception e) {
    		return "cannot create handler " + cn;
    	}
		if(!(h instanceof XActionHandler)) {
			return "cannot reduce handler to XActionHandler";
		}
    	if(h instanceof DefaultSpecialHandler) {
    		String scn = getProperty("support");
    		Class scls = ModelFeatureFactory.getInstance().getFeatureClass(scn);
        	if(scls == null) {
        		return "cannot load support class " + scn;
        	}
        	Object so = null;
        	try {
        		so = scls.newInstance();
        	} catch (Exception e) {
        		return "cannot create support " + scn;
        	}
        	if(!(so instanceof SpecialWizardSupport)) {
        		return "cannot reduce support " + scn + " to SpecialWizardSupport";
        	}
    	}
    	return null;
    }
    
    public String testEntityData() {
    	if(data == null) return null;
    	for (int i = 0; i < data.length; i++) {
    		XEntityDataImpl d = (XEntityDataImpl)data[i];
    		if(d.getModelEntity() == null) {
    			return "cannot find entity " + d.getEntityName();
    		}
    		XAttributeData[] ad = d.getAttributeData();
    		for (int j = 0; j < ad.length; j++) {
    			XAttributeDataImpl a = (XAttributeDataImpl)ad[j];
    			if(a.getAttribute() == null) {
    				return "cannot find attribute " + d.getEntityName() + ":" + a.getAttributeName();
    			}
    		}
    	}
    	return null;
    }
    
}

class ClassHolder2 {
    private XMetaElementImpl owner = null;
    private String name = null;
    private String map = null;

    public ClassHolder2(XMetaElementImpl owner, String name, String map) {
        this.owner = owner;
        this.name = name;
        this.map = map;
    }

    public Object createInstance() {
        if(name == null) return null;
        validate();
        try {
            return ModelFeatureFactory.getInstance().createFeatureInstance(name);
        } catch (Exception e) {
        	name = null;
            return null;
        }
    }

    private void validate() {
        if(map == null) return;
        name = owner.expand(name, map);
        map = null;
    }
    
}
