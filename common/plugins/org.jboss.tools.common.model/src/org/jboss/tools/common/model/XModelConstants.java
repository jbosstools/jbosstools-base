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
	public static String HOME = "product.home"; //$NON-NLS-1$

	public static String WORKSPACE = "workspace.home"; //$NON-NLS-1$
	public static String WORKSPACE_REF = "%" + WORKSPACE + "%"; //$NON-NLS-1$ //$NON-NLS-2$

	public static String WORKSPACE_OLD = "redhat.workspace"; //$NON-NLS-1$
	public static String WORKSPACE_OLD_REF = "%" + WORKSPACE_OLD + "%"; //$NON-NLS-1$ //$NON-NLS-2$

    public static String MODEL_VERSION = "version"; //$NON-NLS-1$
    
    public static String XMODEL_ENTITY_ATTR = "model-entity"; //$NON-NLS-1$
    public static String XMODEL_ENTITY_ATTR_OLD = "ENTITY"; //$NON-NLS-1$
    
    public static String AUTOLOAD = "autoload"; //$NON-NLS-1$

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

    //

    public static void validate(XModel model) {
        validate(model.getProperties());
    }

    public static void validate(Properties p) {
        String h1 = p.getProperty(HOME);
        String w1 = p.getProperty(WORKSPACE);
        //
    }

}
