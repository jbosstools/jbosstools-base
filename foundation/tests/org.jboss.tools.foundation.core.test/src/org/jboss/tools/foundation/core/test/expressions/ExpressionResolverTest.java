/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.foundation.core.test.expressions;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Collection;
import java.util.Properties;

import junit.framework.Assert;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.foundation.core.expressions.ExpressionResolutionException;
import org.jboss.tools.foundation.core.expressions.ExpressionResolver;
import org.jboss.tools.foundation.core.test.FoundationTestActivator;
import org.junit.Test;

public class ExpressionResolverTest {

	ExpressionResolver resolver = new ExpressionResolver();
	
	@Test
    public void testResolverWithBlankExpression() {
        assertEquals("", resolver.resolve(""));
    }

    @Test
    public void testResolverWithNullExpression() {
        assertEquals(null,resolver.resolve(null));
    }

    @Test
    public void testCanonicalResolver() {
        assertEquals("some expression", resolver.resolve("some expression"));
    }

    /**
     * Test that a valid expression to a system property reference which has
     * no definition throws an ISE
     * @throws ExpressionResolutionException 
     */
    @Test(expected = ExpressionResolutionException.class)
    public void testUnresolvedReference() {
        String value = "${no-such-system-property}";
        String resolved = resolver.resolve(value);
        fail("Did not fail with ISE: "+resolved);
    }

    /**
     * Test that a incomplete expression to a system property reference throws an ISE
     * @throws ExpressionResolutionException 
     */
    @Test(expected = ExpressionResolutionException.class)
    public void testIncompleteReference() {
        System.setProperty("test.property1", "test.property1.value");
        try {
        	String value = "${test.property1";
        	String resolved = resolver.resolve(value);
        	fail("Did not fail with ISE: "+resolved);
        } finally {
            System.clearProperty("test.property1");
        }
    }

    /**
     * Validate a single system property expression sees the system property value.
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testSystemPropertyRef() {
        System.setProperty("test.property1", "test.property1.value");
        try {
            String value = "${test.property1}";
            String result = resolver.resolve(value);
            assertEquals("test.property1.value", result);
        } finally {
            System.clearProperty("test.property1");
        }
    }

    /**
     * Test an expression that contains more than one system property name to
     * see that the second property value is used when the first property
     * is not defined.
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testSystemPropertyRefs() {
        System.setProperty("test.property2", "test.property2.value");
        try {
            String value = "${test.property1,test.property2}";
            assertEquals("test.property2.value", resolver.resolve(value));
        } finally {
            System.clearProperty("test.property2");
        }
    }


    /**
     * Test an expression that contains more than one system property name to
     * see that both are used when both are defined
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testSystemPropertyRefs2() {
        System.setProperty("test.property2", "test.property2.value");
        System.setProperty("test.property1", "test.property1.value");
        try {
            String value = "${test.property1,test.property2}";
            assertEquals("test.property1.value", resolver.resolve(value));
        } finally {
            System.clearProperty("test.property1");
            System.clearProperty("test.property2");
        }
    }
    
    /**
     * Validate that a system property expression for a property with no value
     * and a default provides sees the default value.
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testSystemPropertyRefDefault() {
        final String value = "${test.property2:test.property2.default.value}";
        assertEquals("test.property2.default.value", resolver.resolve(value));
    }

    /** 
     * Validate that properties separated by a comma will be tested sequentially
     * until a result is found. 
     *  
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testCommaSeparationProperty() {
    	 final String value = "${test.property1,test.property2:defaultValue}";
    	 assertEquals(resolver.resolve(value),"defaultValue");
    	 try {
	         System.setProperty("test.property2", "test.property2.value");
	    	 assertEquals(resolver.resolve(value),"test.property2.value");
	         System.setProperty("test.property1", "test.property1.value");
	    	 assertEquals(resolver.resolve(value),"test.property1.value");
    	 } finally {
             System.clearProperty("test.property1");
             System.clearProperty("test.property2");
    	 }
    }
    
    /**
     * Validate that a environment variable reference is resolved.
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testSystemEnvVarRef() {
        // Since we cannot set ENV vars from java, grab first one
        String[] envvar = findEnvVar();
        if (envvar[0].length() == 0) {
        	FoundationTestActivator.getDefault().getLog().log(
        			new Status(IStatus.WARNING, FoundationTestActivator.PLUGIN_ID, ("No environment variables found, can't pass test.")));
            return;
        }
        final String envvarValue = envvar[1];
        Assert.assertNotNull("Expect non-null env var: "+envvar[0], envvarValue);
        final String value = "${"+envvar[0]+"}";
        assertEquals(envvarValue, resolver.resolve(value));
    }
    /**
     * Validate that a environment variable reference is overriden by a
     * system property of the same name prefixed with "env.".
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testSystemEnvVarRefOverride() {
        // Since we cannot set ENV vars from java, grab first one
        String[] envvar = findEnvVar();
        if (envvar[0].length() == 0) {
        	FoundationTestActivator.getDefault().getLog().log(
        			new Status(IStatus.WARNING, FoundationTestActivator.PLUGIN_ID, ("No environment variables found, can't pass test.")));
            return;
        }
        // Override the var
        String sysPropName = envvar[0];
        String overrideValue = sysPropName+"-override";
        try {
            System.setProperty(sysPropName, overrideValue);
            final String envvarValue = envvar[1];
            Assert.assertNotNull("Expect non-null env var: "+envvar[0], envvarValue);
            String value = "${"+envvar[0]+"}";
            assertEquals(overrideValue, resolver.resolve(value));
        } finally {
            System.clearProperty(sysPropName);
        }
    }
    
    /** 
     * Make sure to work with local configured properties
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testLocalProperties() {
    	Properties p = new Properties();
    	p.setProperty("foo", "fooValue");
    	
    	ExpressionResolver localResolver = new ExpressionResolver(p);
    	
    	assertEquals("fooValue",localResolver.resolve("${foo:default}"));
    	
    	assertEquals("default", localResolver.resolve("${baz:default}"));

    	assertEquals("System properties should not be honored", "fooValue", localResolver.resolve("${os.name,foo}"));

    }

    /** 
     * Make sure to work with local configured properties that delegates to system properties
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testLocalDelegatedToSystemProperties() {
    	Properties p = new Properties(System.getProperties());
    	p.setProperty("foo", "fooValue");
    	
    	ExpressionResolver localResolver = new ExpressionResolver(p);
    	
    	assertEquals("fooValue",localResolver.resolve("${foo:default}"));
    	assertEquals("default", localResolver.resolve("${baz:default}"));
    	assertEquals("System properties should be honored", System.getProperty("os.name"), localResolver.resolve("${os.name,foo}"));

    }
    /**
     * Make sure nesting works right
     * @throws ExpressionResolutionException 
     */
    @Test
    public void testPatternsWithBrackets() {
        assertEquals("{blah}", resolver.resolve("${resolves.to.nothing:{blah}}"));
        assertEquals("{blah}", resolver.resolve("${resolves.to.nothing,also.resolves.to.nothing:{blah}}"));
        assertEquals(System.getProperty("os.name"), resolver.resolve("${os.name:{blah}}"));
        assertEquals("{{fo{o}oo}}", resolver.resolve("${resolves.to.nothing:{{fo{o}oo}}}"));
        assertEquals("blah{{fo{o}oo}}blah", resolver.resolve("${resolves.to.nothing:blah{{fo{o}oo}}}blah"));
    }
    
    /**
     * Find the first defined System.getenv() environment variable with a non-zero length value.
     * @return [0] = env var name prefixed with "env."
     *  [1] = env var value. If [0].length == 0, then there
     *  were no environment variables defined.
     */
    private static String[] findEnvVar() {
        String[] pair = {"", null};
        Collection<String> envvars = System.getenv().keySet();
        if (envvars.isEmpty()) {
            return pair;
        }
        for(final String envvar : envvars) {
            final String envvarValue = System.getenv(envvar);
            if (envvarValue != null && envvarValue.length() > 0) {
                // Change name to env.name
                pair[0] = "env." + envvar;
                pair[1] = envvarValue;
                break;
            }
        }
        return pair;
    }
}
