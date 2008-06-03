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
package org.jboss.tools.common.meta.impl.documentation;

import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.*;

public class MetaValidatorHandler extends AbstractHandler {

    public MetaValidatorHandler() {}

    public void executeHandler(XModelObject object, java.util.Properties p) {
        if(!isEnabled(object)) return;
        new MetaValidator().validate(object);
    }

    public boolean isEnabled(XModelObject object) {
        return (object != null && "MetaRoot".equals(object.getModelEntity().getName()));
    }

}

