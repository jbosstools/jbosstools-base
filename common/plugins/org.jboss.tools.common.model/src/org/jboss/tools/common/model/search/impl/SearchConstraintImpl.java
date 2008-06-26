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
package org.jboss.tools.common.model.search.impl;

import org.jboss.tools.common.model.search.*;
import org.jboss.tools.common.model.options.*;
import org.jboss.tools.common.model.options.impl.*;
import org.jboss.tools.common.model.*;

public class SearchConstraintImpl extends SharableElementImpl implements SearchConstraint {
	private static final long serialVersionUID = 1L;
    protected SearchConstraint[] constraints;
    protected boolean not;

    public SearchConstraintImpl() {}

    public SearchConstraint[] getConstraints() {
        return constraints;
    }

    public boolean accepts(XModelObject object) {
        return true;
    }

    public void prepare() {
        not = "true".equals(getAttributeValue("not"));
        SharableElement[] cs = getSharableChildren();
        constraints = new SearchConstraint[cs.length];
        for (int i = 0; i < cs.length; i++) {
            constraints[i] = (SearchConstraint)cs[i];
            constraints[i].prepare();
        }
    }

}
