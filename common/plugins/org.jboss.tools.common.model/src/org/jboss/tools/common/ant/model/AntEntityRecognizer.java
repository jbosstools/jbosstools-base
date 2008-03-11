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
package org.jboss.tools.common.ant.model;

import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.ant.parser.*;

public class AntEntityRecognizer implements EntityRecognizer {

    public AntEntityRecognizer() {}

    public String getEntityName(String ext, String body) {
        if(body == null) return null;
        AntParser p = new AntParser(body);
        return (p.getTargets() != null) ? "FileANT" : null;
    }

}
