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
package org.jboss.tools.common.model;

public interface XFilteredTreeConstraint {
    /* Loads options from model
     */
    public void update(XModel model);

    /* Returns true if all children of the node are to be hidden.
     * Has better to return false if there is no fast algorithm,
     * avoiding to call methods getChildren() and getChildByPath().
     */
    public boolean isHidingAllChildren(XModelObject object);

    /* Returns true if at least one child is to be hidden
     */
    public boolean isHidingSomeChildren(XModelObject object);

    /* Returns false if the object is to be hidden.
     */
    public boolean accepts(XModelObject object);

}

