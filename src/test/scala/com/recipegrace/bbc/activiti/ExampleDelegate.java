package com.recipegrace.bbc.activiti;

import org.activiti.engine.delegate.DelegateExecution;
import org.activiti.engine.delegate.Expression;
import org.activiti.engine.delegate.JavaDelegate;

/**
 * Created by Ferosh Jacob on 12/29/16.
 */
public class ExampleDelegate implements JavaDelegate {
    private Expression url;


    @Override
    public void execute(DelegateExecution execution) throws Exception {
        System.out.println(url.getValue(execution));
    }
}
