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

import java.io.*;
import java.util.*;
import java.net.*;

import org.eclipse.core.runtime.FileLocator;
import org.w3c.dom.*;
import org.xml.sax.InputSource;

import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;

public class MetaLibLoader {
	public static String DOC_PUBLICID = "-//Red Hat, Inc.//DTD Meta 1.0//EN"; 

    static {
        try {
            Class c = MetaLibLoader.class;
            XMLEntityResolver.registerPublicEntity(DOC_PUBLICID, FileLocator.resolve(c.getResource("/meta/meta.dtd")).toString());
        } catch (Exception e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
    }
    
	
	private XModelMetaDataImpl meta = null;
//    private String prefix = "meta/";
//    private String suffix = ".meta";
    private HashSet<String> metas = new HashSet<String>();
    private ArrayList<ModuleRef> metarefs = new ArrayList<ModuleRef>();

    public MetaLibLoader() {}

    public void load(XModelMetaDataImpl meta) {
        this.meta = meta;
        try {
			Map<String,URL> resources = MetaResourceLoader.getMetaResources();
			Iterator<String> it = resources.keySet().iterator();
			while(it.hasNext()) {
				String path = it.next();
				URL url = resources.get(path);
				load(path, url);
			}
        } catch (Exception t) {
        	ModelPlugin.getPluginLog().logError("Error in loading meta model resources", t);
        }

        for (int i = 0; i < metarefs.size(); i++) {
            ModuleRef r = metarefs.get(i);
            load(r.element, r.name, r.info);
        }
    }

    void sift(Set modules) {
        boolean b = true;
        while(b) {
            b = false;
            for (int i = metarefs.size() - 1; i >= 0; i--) {
                ModuleRef r = metarefs.get(i);
                if(r.acceptable(modules)) continue;
                b = true;
                metarefs.remove(i);
            }
        }
    }
    
    void load(String name, URL url) {
		InputStream stream = null;
		try {
			stream = url.openStream();
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError("MetaLoader: Cannot read resource " + url.toString());
			return;
		}
		Element g = XMLUtil.getElement(stream);
		if(g == null) {
			ModelPlugin.getPluginLog().logInfo("Corrupted meta resource " + name);
		} else try {
			load0(g, name, url.toString());
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		
		try {
			stream = url.openStream();
			InputSource is = new InputSource(stream);
			String[] errors = XMLUtil.getXMLErrors(is, true);
			if(errors != null && errors.length > 0) {
				ModelPlugin.getPluginLog().logInfo("Errors in " + name);
				for (int i = 0; i < errors.length && i < 5; i++) {
					ModelPlugin.getPluginLog().logInfo(errors[i]);
				}
			}
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		
    }

    void load0(Element g, String name, String source) {
        metarefs.add(new ModuleRef(g, name, source));
    }

    void load(Element g, String name, String source) {
        source = source.substring(0, source.length() - name.length());
        if(metas.contains(name)) {
//            ModelPlugin.log("Can't load module " + name + " second time from " + source);
        } else if(g == null) {
            //ModelPlugin.log("Can't load module " + name + " from " + source);
        } else {
            XMetaDataLoader.loadEntityGroup(meta, g);
            metas.add(name);
            //ModelPlugin.log("Module " + name + " loaded from " + source);
            meta.getLoadedModules().put(name, source);
        }
    }

}

class ModuleRef {
    Element element;
    String name;
    String info;
    String key = "";
    ArrayList<String> depends = new ArrayList<String>();

    public ModuleRef(Element e, String name, String info) {
        this.element = e;
        Element v = XMLUtil.getUniqueChild(e, "VERSION");
        this.name = name;
        this.info = info;
        if(v == null) return;
        if(v.hasAttribute("MODULE") && v.hasAttribute("VERSION")) {
            key =  v.getAttribute("MODULE") + ":" + v.getAttribute("VERSION");
        }
        if(v.hasAttribute("DEPENDS")) {
            String s = v.getAttribute("DEPENDS");
            StringTokenizer st = new StringTokenizer(s, ",");
            while(st.hasMoreTokens()) depends.add(st.nextToken());
        }
    }
/*
    public boolean acceptable(Set modules) {
        if(key.length() == 0 || !modules.contains(key)) {
            ModelPlugin.log("Meta source " + info + " is not covered by license.");
            return false;
        }
        for (int i = 0; i < depends.size(); i++) {
            String k = (String)depends.get(i);
            if(modules.contains(k)) continue;
            if(modules.contains(key)) {
                modules.remove(key);
                ModelPlugin.log("Module " + key + " is not covered by license.");
            }
            ModelPlugin.log("Meta source " + info + " is not covered by license.");
            return false;
        }
        return true;
    }
*/
    public boolean acceptable(Set modules) {
        return (true || key.endsWith("1.0"));
    }

}

