/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.model.ui.dnd;

import org.eclipse.swt.events.TypedEvent;
import org.eclipse.ui.IEditorPart;
import org.eclipse.wst.sse.ui.internal.TextDropAction;

/**
 * @author Evgenij Stherbin
 *
 */
@SuppressWarnings("restriction")
public class VpeTextDropAction extends TextDropAction {


    public boolean run(TypedEvent event, IEditorPart targetEditor) {
        return super.insert((String) event.data, targetEditor);
    }

}
