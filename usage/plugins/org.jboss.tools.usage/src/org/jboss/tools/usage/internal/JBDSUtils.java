/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal;

import org.eclipse.core.runtime.IProduct;
import org.eclipse.core.runtime.Platform;


public class JBDSUtils {

	private static Boolean isJBDS = null;

	public static boolean isJBDS() {
		if (isJBDS == null) {
			IProduct product = Platform.getProduct();
			isJBDS = (product != null
					&& JBDSConstants.JBDS_PRODUCT_ID.equals(product.getId()));
		}
		return isJBDS;
	}
}
