Ext.define('SecondDemo.model.Employee', {
    extend: 'Ext.data.Model',

    fields: [
        { name: 'EMP_NO', type: 'int' },
        { name: 'FIRST_NAME', type: 'string' },
        { name: 'LAST_NAME', type: 'string' },
        { name: 'PHONE_EXT', type: 'string', allowNull: true },
        { name: 'HIRE_DATE', type: 'date' },
        { name: 'DEPT_NO', type: 'string', allowNull: true },
        { name: 'JOB_CODE', type: 'int', allowNull: true },
        { name: 'JOB_GRADE', type: 'string', allowNull: true },
        { name: 'JOB_COUNTRY', type: 'string', allowNull: true },
        { name: 'SALARY', type: 'number', allowNull: true },
        { name: 'FULL_NAME', type: 'string', allowNull: true }

    ],

    idProperty: 'EMP_NO',

    proxy: {
        type: 'rest',
        headers: {
          'Accept': 'application/json'
        },
        url: 'http://localhost:8080/rest/default/main/employee',
        reader: {
            type: 'json',
            rootProperty: 'users'
        }
    }

});