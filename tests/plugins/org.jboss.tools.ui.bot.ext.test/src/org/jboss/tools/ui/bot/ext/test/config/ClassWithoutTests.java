 /*******************************************************************************
  * Copyright (c) 2007-2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.test.config;

import org.jboss.tools.ui.bot.ext.config.Annotations.Require;
/**
 * Test to see what happens when no class annotated with @Require 
 * annotation is run
 * @author lzoubek
 *
 */
@Require()
public class ClassWithoutTests {

}
