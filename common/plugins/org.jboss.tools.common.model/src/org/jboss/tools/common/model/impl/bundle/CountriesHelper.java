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
package org.jboss.tools.common.model.impl.bundle;

import java.net.URL;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;

import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.constraint.impl.XAttributeConstraintAList;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.util.FileUtil;

public class CountriesHelper {
    public static Map<String,String> countries = null;
    public static Map<String,String> languages = null;
    
    static Object initMonitor = new Object();

    public static void init(XModel model) {
        if(countries != null) return;
        synchronized (initMonitor) {
			if(countries != null) return;
        	countries = new TreeMap<String,String>();
        }
        countries.put("", "default");
        languages = new TreeMap<String,String>();
        languages.put("", "default");
        loadMap(countries, "meta/countries.txt");
        loadMap(languages, "meta/languages.txt");
        loadEntity(model, "ValidationFormset");
        loadEntity(model, "ValidationFormset11");
//		loadEntity(model, "BundleList");
    }

    private static void loadEntity(XModel model, String entityName) {
        XModelEntity entity = model.getMetaData().getEntity(entityName);
        if(entity == null) return;
        loadAttribute(countries, entity, "country");
        loadAttribute(languages, entity, "language");
    }

    private static void loadAttribute(Map<String,String> map, XModelEntity entity, String attr) {
        try {
            XAttributeConstraintAList acl = (XAttributeConstraintAList)entity.getAttribute(attr).getConstraint();
            acl.setValues((String[])map.keySet().toArray(new String[0]));
        } catch (Exception e) {
        	ModelPlugin.getPluginLog().logError("CountiesHelper:loadAttribute:" + e.getMessage());
        }
    }

    private static void loadMap(Map<String,String> map, String resource) {
        try {
            URL url = CountriesHelper.class.getClassLoader().getResource(resource);
            String s = FileUtil.readStream(url.openStream());
            StringTokenizer st = new StringTokenizer(s, "\n");
            while(st.hasMoreTokens()) {
                String t = st.nextToken();
                int i = t.indexOf('.'), j = t.lastIndexOf('.');
                if(i < 0) continue;
                String code = t.substring(0, i), name = t.substring(j + 1);
                map.put(code, name);
            }
        } catch (Exception e) {
        	ModelPlugin.getPluginLog().logError("CountiesHelper:loadMap:" + e.getMessage());
        }
    }

} 