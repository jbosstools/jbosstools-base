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
package org.jboss.tools.common.model.loaders.impl;

import java.util.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class ModelEntityRecognizer implements EntityRecognizer {
    private XModelMetaData meta = null;
    private HashMap<String,Object> ext_rec = new HashMap<String,Object>();
    private HashMap<String,EntityRecognizer> cls_rec = new HashMap<String,EntityRecognizer>();

    public ModelEntityRecognizer() {}

    public void setMetaData(XModelMetaData meta) {
        if(this.meta != null) return;
        this.meta = meta;
        load();
    }

    public String getEntityName(String ext, String body) {
    	if(ext != null) ext = ext.toLowerCase();
        EntityRecognizer[] list = (EntityRecognizer[])ext_rec.get(ext);
        if(list == null || list.length == 0) return "FileAny";
        for (int i = 0; i < list.length; i++) {
            String n = list[i].getEntityName(ext, body);
            if(n != null) return n;
        }
        return null;
    }

    private void load() {
        XMapping m = meta.getMapping("Recognizers");
        if(m == null) return;
        String[] keys = m.getKeys();
        for (int i = 0; i < keys.length; i++) {
            String k = keys[i];
            EntityRecognizer r = find(m.getValue(k));
            if(r == null) continue;
            int d = k.indexOf('$');
            String ext = (d < 0) ? k : k.substring(0, d);
            RL rl = findList(ext);
            int p = (d < 0) ? 0 : parsePriority(k.substring(d + 1));
            rl.add(r, p);
        }
        String[] ks = (String[])ext_rec.keySet().toArray(new String[0]);
        for (int i = 0; i < ks.length; i++) {
            RL rl = (RL)ext_rec.get(ks[i]);
            EntityRecognizer[] rs = rl.list();
            if(rs.length == 0) ext_rec.remove(ks[i]);
            else ext_rec.put(ks[i], rs);
        }
    }

    private RL findList(String ext) {
        RL rl = (RL)ext_rec.get(ext);
        if(rl == null) {
            rl = new RL();
            ext_rec.put(ext, rl);
        }
        return rl;
    }

    private EntityRecognizer find(String clsname) {
        EntityRecognizer r = (EntityRecognizer)cls_rec.get(clsname);
        if(r != null) return r;
        try {
        	r = (EntityRecognizer)ModelFeatureFactory.getInstance().createFeatureInstance(clsname);
			cls_rec.put(clsname, r);
			return r;
		} catch (Exception e) {
			ModelPlugin.log("ModelEntityRecognizer:Cannot load recognizer " + clsname);
		}
		return null;
    }

    private int parsePriority(String s) {
        try {
            return (s.length() == 0) ? 0 : Integer.parseInt(s);
        } catch (Exception e) {
            return 10;
        }
    }

    private class RL {
        private Vector<R> v = new Vector<R>(2);

        public void add(EntityRecognizer r, int p) {
            R x = resolve(r, p);
            for (int i = 0; i < v.size(); i++) {
                R y = v.elementAt(i);
                if(x.p < y.p) {
                    v.insertElementAt(x, i);
                    return;
                }
            }
            v.addElement(x);
        }

        private R resolve(EntityRecognizer r, int p) {
            for (int i = 0; i < v.size(); i++) {
                R x = (R)v.elementAt(i);
                if(x.r != r) continue;
                if(p < x.p) x.p = p;
                v.removeElement(x);
                return x;
            }
            return new R(r, p);
        }

        public EntityRecognizer[] list() {
            EntityRecognizer[] rs = new EntityRecognizer[v.size()];
            for (int i = 0; i < v.size(); i++) {
                R x = (R)v.elementAt(i);
                rs[i] = x.r;
            }
            return rs;
        }

    }

    private class R {
        EntityRecognizer r;
        int p;
        R(EntityRecognizer r, int p) {
            this.r = r;
            this.p = p;
        }
    }

}

