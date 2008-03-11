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
package org.jboss.tools.common.verification.vrules.layer;

import java.util.*;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.verification.vrules.VModel;

public class VModelFactory {
	static Map<XModel,VModel> models = new HashMap<XModel,VModel>();
	
	public static VModel getModel(XModel model) {
		VModel m = (VModel)models.get(model);
		if(m == null) {
			m = new VModelImpl(model);
			models.put(model, m);
		}
		return m;
	}	

}
