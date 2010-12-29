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
package demo;

/**
 * Created by JBoss Developer Studio
 */
public class User {

	private String name;

	/**
	 * @return User Name
	 */
	public String getST() {
		return name;
	}

	/**
	 * @param User Name
	 */
	public void setST(String name) {
		this.name = name;
	}

	public boolean isBooleanValue1() {
		return true;
	}

	public Boolean isBooleanValue2() {
		return Boolean.TRUE;
	}

	public int isBooleanValue3() {
		return 0;
	}

	public void getVoid() {
		
	}

	public int getIntValue() {
		return 0;
	}
}