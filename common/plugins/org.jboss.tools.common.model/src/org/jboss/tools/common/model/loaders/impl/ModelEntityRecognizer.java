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
    private HashMap<String,EntityRecognizer[]> recognizers = new HashMap<String,EntityRecognizer[]>();

    public ModelEntityRecognizer() {}
    
    boolean loaded = false;

    public void setMetaData(XModelMetaData meta) {
        if(this.meta != null) return;
        this.meta = meta;
        load();
    }

	public String getEntityName(EntityRecognizerContext context) {
		String ext = context.getExtension();
    	if(ext != null) ext = ext.toLowerCase();
        EntityRecognizer[] list = recognizers.get(ext);
        if(list == null || list.length == 0) return "FileAny"; //$NON-NLS-1$
        for (EntityRecognizer r: list) {
            String n = r.getEntityName(context);
            if(n != null) return n;
        }
        return null;
	}

    private void load() {
        XMapping m = meta.getMapping("Recognizers"); //$NON-NLS-1$
        if(m == null) return;
        HashMap<String,RL> ext_list = new HashMap<String,RL>();
        HashMap<String,EntityRecognizer> cls_recw = new HashMap<String,EntityRecognizer>();
        String[] keys = m.getKeys();
        for (int i = 0; i < keys.length; i++) {
            String k = keys[i];
            String clsname = m.getValue(k);
            if(clsname == null || clsname.trim().length() == 0) continue;
            EntityRecognizer r = cls_recw.get(clsname);
        	if(r == null) {
        		r = new EntityRecognizerWrapper(clsname);
        		cls_recw.put(clsname, r);
        	}
            
            int d = k.indexOf('$');
            String ext = (d < 0) ? k : k.substring(0, d);
            RL rl = ext_list.get(ext);
            if(rl == null) {
                rl = new RL();
                ext_list.put(ext, rl);
            }
            int p = (d < 0) ? 0 : parsePriority(k.substring(d + 1));
            rl.add(r, p);
        }
        String[] ks = ext_list.keySet().toArray(new String[0]);
        for (int i = 0; i < ks.length; i++) {
            RL rl = ext_list.get(ks[i]);
            EntityRecognizer[] rs = rl.list();
            ext_list.remove(ks[i]);
            if(rs.length > 0) {
            	recognizers.put(ks[i], rs);
            }            
        }
    }
    
    private EntityRecognizer find(String clsname) {
        try {
        	return (EntityRecognizer)ModelFeatureFactory.getInstance().createFeatureInstance(clsname);
		} catch (ClassCastException e) {
			ModelPlugin.getPluginLog().logError("Entity recognizer " + clsname + " must be instanceof EntityRecognizer", e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return null;
    }

    private int parsePriority(String s) {
    	if(s == null) return 10;
        try {
            return (s.length() == 0) ? 0 : Integer.parseInt(s);
        } catch (NumberFormatException e) {
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
                R x = v.elementAt(i);
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
    
    private class EntityRecognizerWrapper implements EntityRecognizer {
    	String clsname;
    	EntityRecognizer resolved;
    	
    	public EntityRecognizerWrapper(String clsname) {
    		this.clsname = clsname;
    	}
    	
    	boolean checkResolved() {
			if(resolved == null && clsname != null) {
				resolved = find(clsname);
				clsname = null;
			}
			return resolved != null;
    	}

		public String getEntityName(EntityRecognizerContext context) {
			if(!checkResolved()) return null;
			return resolved.getEntityName(context);
		}
    }

}

