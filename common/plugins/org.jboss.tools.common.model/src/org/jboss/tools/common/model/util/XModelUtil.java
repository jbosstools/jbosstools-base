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
package org.jboss.tools.common.model.util;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.impl.*;

public class XModelUtil {
	private static final String DEFAULT_MODEL_VERSION = "5.0";

    public static void addModifyListener(XModel model, Object listener) {
        RootImpl impl = (RootImpl)model.getRoot();
        impl.addModifyListener(listener);
    }

    public static void addHistoryListener(XModel model, Object listener) {
        model.getUndoManager().addListener(listener);
    }

	public static String getModelVersion(XModel model) {
    	XModelObject fss = FileSystemsHelper.getFileSystems(model);
    	if(fss == null) return DEFAULT_MODEL_VERSION;
		String modelVersionStr = fss.getAttributeValue(XModelConstants.MODEL_VERSION);
		return (modelVersionStr == null || "".equals(modelVersionStr)) ? DEFAULT_MODEL_VERSION : modelVersionStr;
	}
}

