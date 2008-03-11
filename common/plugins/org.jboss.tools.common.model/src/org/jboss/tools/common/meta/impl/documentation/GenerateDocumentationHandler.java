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
///import org.jboss.tools.common.meta.ui.editor.*;

public class GenerateDocumentationHandler extends AbstractHandler {

    public GenerateDocumentationHandler() {}

    public void executeHandler(XModelObject object, java.util.Properties p) throws Exception {
        DocumentGenerator g = new DocumentGenerator();
        g.setModel(object.getModel());
////        String filename = MetaRootLoader.storage(object).getRootFolder() + "meta.html";
////        g.generate(filename);
////        object.getModel().getService().showDialog("Message",
////               "Meta documentation has been saved to\n" + filename,
////               new String[] {"OK"}, null, ServiceDialog.MESSAGE);
    }

    public boolean isEnabled(XModelObject object) {
        return (object != null);
    }

}
