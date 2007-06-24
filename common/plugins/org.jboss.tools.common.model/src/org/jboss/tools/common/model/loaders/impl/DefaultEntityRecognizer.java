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

import java.io.*;
import org.w3c.dom.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.util.XMLUtil;

public class DefaultEntityRecognizer implements EntityRecognizer {

    public DefaultEntityRecognizer() {}

    public String getEntityName(String ext, String body) {
        if(body == null) return null;
        Element e = XMLUtil.getElement(new StringReader(body));
        if(e == null || !XMLUtil.hasAttribute(e, "ENTITY")) return null;
        return e.getAttribute("ENTITY");
    }

}
