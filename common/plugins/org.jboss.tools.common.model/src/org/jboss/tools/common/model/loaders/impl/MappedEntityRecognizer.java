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
import org.jboss.tools.common.model.loaders.*;

public class MappedEntityRecognizer implements EntityRecognizer {
    private Map<String,String> map = new HashMap<String,String>();

    public MappedEntityRecognizer() {
        map.put("bpf", "FileProcess");
        map.put("htm", "FileHTML");
        map.put("flow", "FileFlow");
        map.put("jspx", "FileJSP");
        map.put("jspf", "FileJSP");
    }

    public String getEntityName(String ext, String body) {
    	if(ext == null) return null;
        String s = (String)map.get(ext.toLowerCase());
        return (s != null) ? s : "File" + ext.toUpperCase();
    }

}
