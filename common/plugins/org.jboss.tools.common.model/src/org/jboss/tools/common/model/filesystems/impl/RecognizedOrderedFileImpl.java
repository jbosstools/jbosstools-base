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
package org.jboss.tools.common.model.filesystems.impl;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.impl.*;

public class RecognizedOrderedFileImpl extends RecognizedFileImpl {
    private static final long serialVersionUID = 4508932464835442042L;

    public RecognizedOrderedFileImpl() {}

    protected RegularChildren createChildren() {
        return new OrderedChildren();
    }

    protected java.util.Comparator<XModelObject> createComparator() {
        return null;
    }

}

