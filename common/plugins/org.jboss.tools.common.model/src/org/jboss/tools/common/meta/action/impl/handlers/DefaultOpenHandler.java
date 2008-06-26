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
package org.jboss.tools.common.meta.action.impl.handlers;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.impl.*;

public class DefaultOpenHandler extends AbstractHandler {

    public DefaultOpenHandler() {}
    
    public boolean isEnabled(XModelObject object) {
        boolean abs = "abstract".equals(object.getAttributeValue("override"));
        return !abs;
    }

}
