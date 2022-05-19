/*******************************************************************************
 * Copyright (c) 2022 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.el.internal.core.parser.token;

/**
 *
 * @author T. Wellpott
 *
 */
public class LambdaTokenDescription extends ConstantTokenDescription {
	public static final int LAMBDA = 19;

	public static LambdaTokenDescription INSTANCE = new LambdaTokenDescription();

	public LambdaTokenDescription () {
		super("->", LAMBDA); //$NON-NLS-1$
	}

}
