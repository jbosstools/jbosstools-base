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

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.search.*;

public class SearchConstraintORImpl extends SearchConstraintImpl {

    public SearchConstraintORImpl() {}

    public boolean accepts(XModelObject object) {
        SearchConstraint[] sc = getConstraints();
        for (int i = 0; i < sc.length; i++)
          if(sc[i].accepts(object)) return !not;
        return (sc.length == 0) ? !not : not;
    }

}
