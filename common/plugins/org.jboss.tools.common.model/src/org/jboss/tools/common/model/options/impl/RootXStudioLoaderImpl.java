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
package org.jboss.tools.common.model.options.impl;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.loaders.*;

public class RootXStudioLoaderImpl implements XObjectLoader {

    public RootXStudioLoaderImpl() {}

    public void load(XModelObject object) {
        new XStudioDataLoaderImpl().load(object);
    }

    public boolean update(XModelObject object) {
        return true;
    }

    public boolean save(XModelObject object) {
        return new XStudioDataLoaderImpl().save(object);
    }

}
