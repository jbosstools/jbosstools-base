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

import java.util.*;
import java.text.*;

import org.jboss.tools.common.meta.key.WizardKeys;

public class XBundle {
    private static XBundle bundle = new XBundle();
	
    private static ResourceBundle messages = ResourceBundle.getBundle(XBundle.class.getName());
    private static String ERR_GET_MESSAGE        = "ERR_GET_MESSAGE";
    private static String ERR_TEMPLATE_NOT_FOUND = "ERR_TEMPLATE_NOT_FOUND"; 
    
    private XBundle() {}

    public static XBundle getInstance() {
        return bundle;
    }

    private String findTemplate(String resourceid, String templateid) {
        try {
            return WizardKeys.getString(templateid);
        } catch (Exception e) {
            return MessageFormat.format(messages.getString(XBundle.ERR_TEMPLATE_NOT_FOUND),new Object[]{templateid,resourceid});
        }
    }

    public String getMessage(String resourceid, String templateid) {
        return getMessage(resourceid, templateid, null);
    }

    public String getMessage(String resourceid, String templateid, Object[] args) {
        String t = findTemplate(resourceid, templateid);
        if(args == null) return t;
        try {
            return MessageFormat.format(t, args);
        } catch (Exception e) {
            return MessageFormat.format(messages.getString(XBundle.ERR_GET_MESSAGE),new Object[]{resourceid,templateid,t});
	    }
    }

}

