/*
 * VRuleSetsLoader.java
 * Created on July 23, 2003, 4:42 PM
 */

package org.jboss.tools.common.verification.vrules.model;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.core.runtime.FileLocator;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.engines.impl.EnginesLoader;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.XMLUtil;
import org.jboss.tools.common.verification.vrules.plugin.VerificationPlugin;
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
    	if("true".equals(object.getModel().getProperties().getProperty("initialModel"))) return; //$NON-NLS-1$ //$NON-NLS-2$
        super.load(object);
        Set<String> installed = new HashSet<String>();
        
        Set resources = RuleSetResourceLoader.getResources("org.jboss.tools.common.verification.rules"); //$NON-NLS-1$

		ArrayList<URL> l = new ArrayList<URL>();
		Iterator it = resources.iterator();
		while(it.hasNext()) {
			URL url = (URL)it.next();
			try { 
				if(url != null) url = FileLocator.resolve(url);
				if(url != null) l.add(url);
			} catch (IOException e) {
				ModelPlugin.getPluginLog().logError(e);
			}
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
                        	//done to provide 'default-enabled'
                        	mergeRules(c, c);
                    } else {
                        mergeRules(c, nc[j]);
                    }
                    c.setAttributeValue("installed", "true"); //$NON-NLS-1$ //$NON-NLS-2$
                    installed.add(c.getPathPart());
                }
            } catch (XModelException e) {
				VerificationPlugin.getPluginLog().logError(e);
            } catch (IOException e) {
				VerificationPlugin.getPluginLog().logError(e);
			}
        }
        XModelObject[] ch = object.getChildren();
        for (int i = 0; i < ch.length; i++) {
            String ins = ch[i].getAttributeValue("installed"); //$NON-NLS-1$
            if ("true".equals(ins)) { //$NON-NLS-1$
                if (!installed.contains(ch[i].getPathPart())) {
                    ch[i].removeFromParent();
                }
            }
        }
    }
    
    static int i = 0;

    protected void mergeRules(XModelObject object, XModelObject update) throws XModelException {
        org.jboss.tools.common.meta.XAttribute[] as = object.getModelEntity().getAttributes();
        for (int i = 0; i < as.length; i++) {
            String n = as[i].getName();
            if ("enabled".equals(n)) { //$NON-NLS-1$
            	String nv = update.getAttributeValue(n);
            	object.set("default-enabled", nv); //$NON-NLS-1$
            }
            if ("enabled".equals(n) || "installed".equals(n)) continue; //$NON-NLS-1$ //$NON-NLS-2$
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
        return ".rule-sets.xml"; //$NON-NLS-1$
    }
    
}
