/*
 * VRuleSetsLoader.java
 * Created on July 23, 2003, 4:42 PM
 */

package org.jboss.tools.common.verification.vrules.model;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.engines.impl.EnginesLoader;
import org.jboss.tools.common.verification.vrules.plugin.VerificationPlugin;

import java.io.*;
import java.net.*;
import java.util.*;

import org.eclipse.core.runtime.FileLocator;
import org.w3c.dom.Element;

/**
 * @author  valera
 */
public class VRuleSetsLoader extends EnginesLoader {
    
    /** Creates a new instance of VRuleSetsLoader */
    public VRuleSetsLoader() {
        util.setup(null, false);
    }
    
    public void load(XModelObject object) {
    	if("true".equals(object.getModel().getProperties().getProperty("initialModel"))) return;
        super.load(object);
        Set<String> installed = new HashSet<String>();
        
        Set resources = RuleSetResourceLoader.getResources("org.jboss.tools.common.verification.rules");

		ArrayList<URL> l = new ArrayList<URL>();
		Iterator it = resources.iterator();
		while(it.hasNext()) {
			URL url = (URL)it.next();
			try { 
				if(url != null) url = FileLocator.resolve(url);
				if(url != null) l.add(url);
			} catch (Exception e) {}
		}
		it = l.iterator();
        while(it.hasNext()) {
			try {
				URL url = (URL)it.next();
                InputStream is = url.openConnection().getInputStream();
                Element element = XMLUtil.getElement(is);
                if (element == null) continue;
                XModelObject copy = object.copy(0);
                util().load(element, copy);
                
                XModelObject[] nc = copy.getChildren();
                for (int j = 0; j < nc.length; j++) {
                    XModelObject c = object.getChildByPath(nc[j].getPathPart());
                    if (c == null) {
                        c = nc[j].copy();
                        object.addChild(c);
                    } else {
                        mergeRules(c, nc[j]);
                    }
                    c.setAttributeValue("installed", "true");
                    installed.add(c.getPathPart());
                }
            } catch (Exception e) {
				if(VerificationPlugin.isDebugEnabled()) {
					VerificationPlugin.getPluginLog().logError(e);
				}
            }
        }
        XModelObject[] ch = object.getChildren();
        for (int i = 0; i < ch.length; i++) {
            String ins = ch[i].getAttributeValue("installed");
            if ("true".equals(ins)) {
                if (!installed.contains(ch[i].getPathPart())) {
                    ch[i].removeFromParent();
                }
            }
        }
    }

    protected void mergeRules(XModelObject object, XModelObject update) {
        org.jboss.tools.common.meta.XAttribute[] as = object.getModelEntity().getAttributes();
        for (int i = 0; i < as.length; i++) {
            String n = as[i].getName();
            if ("enabled".equals(n) || "installed".equals(n)) continue;
            String ov = object.getAttributeValue(n);
            String nv = update.getAttributeValue(n);
            if (ov.equals(nv)) continue;
            object.getModel().changeObjectAttribute(object, n, nv);
        }
        XModelObject[] cs = update.getChildren();
        for (int i = 0; i < cs.length; i++) {
            XModelObject c = object.getChildByPath(cs[i].getPathPart());
            if (c == null) {
                object.addChild(cs[i].copy());
            } else {
				mergeRules(c, cs[i]);
            }
        }
        cs = ((org.jboss.tools.common.model.impl.XModelObjectImpl)object).getChildrenForSave();
        for (int i = 0; i < cs.length; i++) {
            XModelObject c = update.getChildByPath(cs[i].getPathPart());
            if (c == null) cs[i].removeFromParent();
        }
    }
    
	public boolean save(XModelObject object) {
		//in custom version only preference model may save user settings
		if(object.getModel() != PreferenceModelUtilities.getPreferenceModel()) return true;
		return super.save(object);
	}

    protected String fileName(XModelObject object) {
        return XModelConstants.getProjectPrefix(object.getModel()) +
               ".rule-sets.xml";
    }
    
}
