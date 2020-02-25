/*******************************************************************************
 * Copyright (c) 2020 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package demo;

import javax.inject.Named;
import java.io.Serializable;
import javax.enterprise.context.SessionScoped;

/**
 * Created by JBoss Tools
 */
@Named
@SessionScoped
public class User  implements Serializable {
	
	private static final long serialVersionUID = 1L;
	private String name;

	public User() {
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String sayHello() {
		return "greeting";
	}
}