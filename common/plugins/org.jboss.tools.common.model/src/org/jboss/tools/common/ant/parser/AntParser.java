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
package org.jboss.tools.common.ant.parser;

import java.io.*;
import org.w3c.dom.*;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.util.XMLUtil;


/**
 * FIXME switch to TargetInfo[] AntRunner.getAvailableTargets() 
 * @see org.eclipse.ant.core.AntRunner#getAvailableTargets()
 */
public class AntParser {
    private String[] targets = null;

    public AntParser(String body) {
        if(body == null || body.indexOf("<project") < 0) return; //$NON-NLS-1$
        Element element = XMLUtil.getElement(new StringReader(body));
        if(element == null) return;
        if(!"project".equals(element.getNodeName())) return; //$NON-NLS-1$
        if(element.hasAttribute("xmlns")) return; //$NON-NLS-1$
        if(element.hasAttribute("xsi:schemaLocation")) return; //$NON-NLS-1$
        NodeList es = element.getElementsByTagName("target"); //$NON-NLS-1$
        for (int i = 0; i < es.getLength(); i++)
          if(!((Element)es.item(i)).hasAttribute(XModelObjectConstants.ATTR_NAME)) return;
        targets = new String[es.getLength()];
        for (int i = 0; i < es.getLength(); i++)
          targets[i] = ((Element)es.item(i)).getAttribute(XModelObjectConstants.ATTR_NAME);
    }

    public String[] getTargets() {
        return targets;
    }

}
