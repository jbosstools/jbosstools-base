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

import java.io.*;
import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.options.*;

class XStudioLoaderPeer implements SharableConstants {
    private static final XStudioLoaderPeer instance = new XStudioLoaderPeer();

    public static XStudioLoaderPeer instance() {
        return instance;
    }

    private Hashtable<String,File[]> paths = new Hashtable<String,File[]>(3);

    private boolean isLoadingOn;

    private XStudioLoaderPeer() {}

    public void init(XModelObject studio) {
        XModel model = studio.getModel();
		String r = XModelConstants.getWorkspace(model) + "/";
        for (int i = 0; i < LIST.length; i++) {
            String[] fns = FILE_LIST[i];
            File[] fs = new File[fns.length];
            for (int j = 0; j < fs.length; j++) fs[j] = new File(r + FILE_LIST[i][j]);
            paths.put(LIST[i], fs);
        }
    }

    void setIsLoadingOn(boolean b) {
        isLoadingOn = b;
    }

    File[] getFilesForScope(String scope) {
        return paths.get(scope);
    }
    
    public File getProjectPreferencesFile() {
		File[] fs = getFilesForScope(SharableConstants.PROJECT);
		return (fs != null && fs.length > 1) ? fs[1] : null;  
    }

    public boolean isScopeEditable(String scope) {
        if(isLoadingOn) return true;
        if(GENERAL.equals(scope)) return false;
        File[] f = getFilesForScope(scope);
        if(f == null) return false;
        for (int i = 0; i < f.length; i++)
        	if(f[i].exists() && !f[i].canWrite()) return false;
        return true;
    }

    // util

    public static String getMaxScope(String s1, String s2) {
        int i1 = range(s1), i2 = range(s2);
        return (i1 > i2) ? s1 : s2;
    }

    private static int range(String scope) {
        for (int i = 0; i < LIST.length; i++)
          if(LIST[i].equals(scope)) return i;
        return -1;
    }

}
