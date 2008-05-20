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
package org.jboss.tools.common.model.event;

import java.util.*;

import org.jboss.tools.common.model.*;

public class XModelChangeManager {
    private ArrayList<OLBind> binds = new ArrayList<OLBind>();
    
    public XModelChangeManager() {}

    public synchronized void createListener(XModelObject object, XModelChangeListener listener) {
        binds.add(new OLBind(object, listener));
    }

    public synchronized void removeListener(XModel model, String path, XModelChangeListener listener) {
        XModelObject object = model.getByPath(path);
        if(object == null) return;
        OLBind bind = new OLBind(object, listener);
        int l = binds.size();
        for (int i = l - 1; i >= 0; i--) {
            if(bind.equals((OLBind)binds.get(i))) binds.remove(i);
        }
    }

    public synchronized void removeListener(XModelChangeListener listener) {
        int l = binds.size();
        for (int i = l - 1; i >= 0; i--) {
            OLBind bind = (OLBind)binds.get(i);
            if(listener == bind.listener) binds.remove(i);
        }
    }

    public boolean canFire() {
        int l = binds.size();
        for (int i = l - 1; i >= 0; i--) {
            OLBind bind = (OLBind)binds.get(i);
            if(bind.canFire()) return true;
        }
        return false;
    }

    public synchronized void fire() {
        int l = binds.size();
        for (int i = l - 1; i >= 0; i--) {
            OLBind bind = (OLBind)binds.get(i);
            bind.fire();
            if(bind.isDead()) binds.remove(i);
        }
    }
    
    public List<OLBind> getBindList() {
    	return Collections.unmodifiableList(binds);
    }
}

class OLBind {
    private XModelObject object = null;
    private String path = null;
    XModelChangeListener listener = null;
    private long timestamp = 0;

    public OLBind(XModelObject object, XModelChangeListener listener) {
        this.object = object;
        path = object.getPath();
        timestamp = object.getTimeStamp();
        this.listener = listener;
    }

    public void fire() {
        if(isDead()) return;
        XModelObject o = object;
        if(!object.isActive()) {
            object = object.getModel().getByPath(path);
        }
        if(isDead()) {
            listener.delete(o);
        } else if(timestamp != object.getTimeStamp()) {
             timestamp = object.getTimeStamp();
             path = object.getPath();
             listener.update(object);
        }
    }

    public boolean isDead() {
        return (object == null);
    }

    public boolean canFire() {
        if(isDead()) return false;
        return (!object.isActive() || timestamp != object.getTimeStamp());
    }

    public boolean equals(OLBind bind) {
        return (object == bind.object && listener == bind.listener);
    }

}

