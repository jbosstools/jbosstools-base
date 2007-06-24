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
package org.jboss.tools.common.model;

import java.util.*;

public class XModelConstants {
	public static String HOME = "redhat.home";
    public static String WORKSPACE = "redhat.workspace";
    public static String WORKSPACE_NAME = "redhat.workspace.name";
    
    public static String MODEL_VERSION = "version";

    private static String ECOM_HOME = "global.ECOM_HOME";
    private static String ECOM_DATA = "global.ECOM_DATA";

    private XModelConstants() {}

    public static String getHome(XModel model) {
        return model.getProperties().getProperty(HOME);
    }

    public static String getWorkspace(XModel model) {
        return model.getProperties().getProperty(WORKSPACE);
    }

    public static void setWorkspace(XModel model, String workspace) {
        model.getProperties().setProperty(WORKSPACE, workspace);
        validate(model);
    }

    public static String getWorkspaceName(XModel model) {
        return model.getProperties().getProperty(WORKSPACE_NAME);
    }

    public static void setWorkspaceName(XModel model, String name) {
        if(name == null)
          model.getProperties().remove(WORKSPACE_NAME);
        else
          model.getProperties().setProperty(WORKSPACE_NAME, name);
    }

    public static String getProjectName(XModel model) {
        String s = getWorkspaceName(model);
        if(s != null) return s;
        s = getWorkspace(model);
        return (s == null) ? null : new java.io.File(s).getName();
    }

    public static String getProjectPrefix(XModel model) {
        String s = getWorkspaceName(model);
        return (s == null) ? "" : s + "-";
    }

    //

    public static void validate(XModel model) {
        validate(model.getProperties());
    }

    public static void validate(Properties p) {
        String h1 = p.getProperty(HOME), h2 = p.getProperty(ECOM_HOME);
        if(h1 != null) p.setProperty(ECOM_HOME, h1);
        else if(h2 != null) p.setProperty(HOME, h2);
        String w1 = p.getProperty(WORKSPACE), w2 = p.getProperty(ECOM_DATA);
        if(w1 != null) p.setProperty(ECOM_DATA, w1);
        else if(w2 != null) p.setProperty(WORKSPACE, w2);
    }

}
