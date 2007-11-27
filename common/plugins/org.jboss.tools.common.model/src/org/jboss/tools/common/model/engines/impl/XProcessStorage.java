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
package org.jboss.tools.common.model.engines.impl;

import java.util.*;

public class XProcessStorage {
    private static XProcessStorage storage = new XProcessStorage();

    public static XProcessStorage getDefaultStorage() {
        return storage;
    }

    private HashMap<String,HashMap<Long,XProcess>> objects = new HashMap<String,HashMap<Long,XProcess>>();

    public XProcessStorage() {}

    public void addInstance(String path, XProcess p) {
        if(p == null || !p.isRunning()) return;
        getInstances(path).put(new Long(System.currentTimeMillis()), p);
    }

    private HashMap<Long,XProcess> getInstances(String path) {
        HashMap<Long,XProcess> is = objects.get(path);
        if(is == null) {
            is = new HashMap<Long,XProcess>();
            objects.put(path, is);
        }
        return is;
    }

    public HashMap<Long,XProcess> getRunningInstances(String path) {
        HashMap<Long,XProcess> is = objects.get(path);
        if(is == null) return null;
        validate(is);
        return (is.size() == 0) ? null : is;
    }

    private void validate(HashMap<Long,XProcess> is) {
        Iterator ks = is.keySet().iterator();
        while(ks.hasNext()) {
            Object k = ks.next();
            XProcess p = is.get(k);
            if(!p.isRunning()) ks.remove();
        }
    }

    public void stopInstance(String path, long time) {
        HashMap<Long,XProcess> is = getRunningInstances(path);
        if(is == null) return;
        Long l = new Long(time);
        XProcess p = is.get(l);
        if(p == null) return;
        p.stop();
        is.remove(l);
    }

}
